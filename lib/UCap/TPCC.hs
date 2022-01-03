module UCap.TPCC where

import UCap.TPCC.Data

import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.UCap
import Data.UCap.Counter
import Data.UCap.Map
import Data.UCap.Op
import Lens.Micro.Platform

type TpccOp = Op' TpccC

newOrder
  :: ReplicaId
  -> CustomerId
  -> [OrderLine]
  -> TpccOp (Either String ())
newOrder rid cid ols = mkOp idC uniC $ \s ->
  case newOrderRs rid cid ols s of
    Right e -> (e, Right ())
    Left s -> (idE, Left s)

payment :: CustomerId -> Int -> TpccOp ()
payment cid amt = mkOp idC uniC $ \s ->
  (idE & tpccCustomers <>~ adjustE cid (idE & cBalance <>~ subE amt), ())
  -- Also increase the YTD of the warehouse and district...

newOrderRs
  :: ReplicaId
  -> CustomerId
  -> [OrderLine]
  -> Tpcc
  -> Either String (CEffect TpccC)
newOrderRs rid cid ols s = do
  -- Find total cost of order
  tcost <- case olTotal s ols of
    Right amt -> return amt
    Left i -> Left $ "Item does not exist: " ++ i
  -- Confirm item availability
  subs <- case olShorts s "w1" ols of
            Right e -> return e
            Left s -> Left $ "Item shortage: " ++ show s
  -- Create unique order id
  let oid = newOid s rid
  let order = mkOrder cid "today" ols
  return $ idE
    -- Charge customer
    & tpccCustomers <>~ adjustE cid (idE & cBalance <>~ addE tcost)
    -- Reduce item quantities
    & (<> subs)
    -- Create new order
    & tpccOrders <>~ insertE oid order

newOid :: Tpcc -> ReplicaId -> OrderId
newOid s rid =
  let nums = map snd . filter ((== rid) . fst) $ Map.keys (s^.tpccOrders)
  in case nums of
       [] -> (rid,0)
       ns -> (rid, maximum ns + 1)

olShorts
  :: Tpcc
  -> WarehouseId
  -> [OrderLine]
  -> Either (ItemId,Int) TpccE -- (MapE (WarehouseId, ItemId) StockE)
olShorts s w = foldM
  (\es ol -> case olShort s w ol of
               Right e -> Right $ es <> e
               Left s -> Left s)
  idE

olShort
  :: Tpcc
  -> WarehouseId
  -> OrderLine
  -> Either (ItemId,Int) TpccE -- (MapE (WarehouseId, ItemId) StockE)
olShort s w ol =
  case s ^. tpccStock . at (w, ol^.olItemId) of
    Just st | st^.sQuantity > ol^.olQuantity ->
              Right $ idE & tpccStock <>~ adjustE (w, ol^.olItemId) (idE & sQuantity <>~ subE (ol^.olQuantity))
            | otherwise -> Left (ol^.olItemId, ol^.olQuantity - st^.sQuantity)
    Nothing -> Left (ol^.olItemId, ol^.olQuantity)

olTotal :: Tpcc -> [OrderLine] -> Either ItemId Int
olTotal s = foldM
  (\a ol -> case olCost s ol of
              Just c -> Right $ a + c
              Nothing -> Left $ ol^.olItemId)
  0

olCost :: Tpcc -> OrderLine -> Maybe Int
olCost s ol = do
  item <- s ^. tpccItems . at (ol^.olItemId)
  return $ ol^.olQuantity * item^.iPrice
