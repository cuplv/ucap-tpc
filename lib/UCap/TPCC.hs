{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.TPCC where

import UCap.TPCC.Data

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (traverse_)
import Data.UCap
import Data.UCap.Op
import Lens.Micro.Platform

type TpccOp = Op' TpccC

newOrder
  :: ReplicaId
  -> CustomerId
  -> [OrderLine]
  -> TpccOp OrderId
newOrder rid cid ols =
  takeItems "w1" ols
  *> chargeCustomer cid ols
  *> insertOrder rid (mkOrder cid "today" ols)

acceptPayment :: CustomerId -> Int -> TpccOp ()
acceptPayment cid amt = mkOp idC uniC $ \s ->
  (idE & tpccCustomers <>~ adjustE cid (idE & cBalance <>~ subE amt), ())
  -- Also increase the YTD of the warehouse and district...

chargeCustomer :: CustomerId -> [OrderLine] -> TpccOp Int
chargeCustomer cid ols = mkOp
  uniC
  (idC & tpccCustomers <>~ adjustC cid (idC & cBalance <>~ addAny))
  (\s ->
     let Right tcost = olTotal s ols
     in (idE & tpccCustomers <>~ adjustE cid (idE & cBalance <>~ addE tcost), tcost))

insertOrder :: ReplicaId -> Order -> TpccOp OrderId
insertOrder rid o = mkOp
  (uniC & tpccOrders /\~ insertAny)
  (idE & tpccOrders <>~ insertAny)
  (\s -> let oid = newOid s rid
         in (idE & tpccOrders <>~ insertE oid o, oid))

newOid :: Tpcc -> ReplicaId -> OrderId
newOid s rid =
  let nums = map snd . filter ((== rid) . fst) $ Map.keys (s^.tpccOrders)
  in case nums of
       [] -> (rid,0)
       ns -> (rid, maximum ns + 1)

takeItems :: WarehouseId -> [OrderLine] -> TpccOp ()
takeItems w ols =
  let irs = itemReqs w ols
      f a = checkItem a *> opEffect (itemE a)
  in traverse_ f irs

itemReqs :: WarehouseId -> [OrderLine] -> [((WarehouseId, ItemId), Int)]
itemReqs w ols = Map.toList $ foldl 
  (\m ol -> Map.alter (g ol) (w, ol^.olItemId) m)
  Map.empty
  ols
  where g ol a = Just $ ol^.olQuantity + (case a of
                                            Just n -> n
                                            Nothing -> 0)

checkItem :: ((WarehouseId, ItemId), Int) -> TpccOp ()
checkItem (i,n) = opTest
  (uniC & tpccStock /\~ adjustC i (uniC & sQuantity /\~ addAny))
  (\s -> case s ^. tpccStock . at i of
           Just e | e^.sQuantity >= n -> True
           _ -> False)

itemE :: ((WarehouseId, ItemId), Int) -> TpccE
itemE (i,n) = idE & tpccStock <>~ adjustE i (idE & sQuantity <>~ subE n)

(/\~) :: (Meet a) => ASetter s t a a -> a -> s -> t
(/\~) l b = l %~ meet b

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
