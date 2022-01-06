{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module UCap.TPCC where

import UCap.TPCC.Data

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (traverse_)
import Data.UCap
import Data.UCap.Lens
import Data.UCap.Op

type TpccOp a = Op' TpccC a

newOrder
  :: ReplicaId
  -> CustomerId
  -> [OrderLine]
  -> TpccOp OrderId
newOrder rid cid ols =
  takeItems "w1" ols
  *> chargeCustomer cid ols
  *> insertOrder rid (mkOrder cid "today" ols)

acceptPayment :: CustomerId -> Int -> TpccOp Int
acceptPayment cid amt = edLift
  (tpccCustomersEd *: keyEd cid *: cBalanceEd)
  (subGuard 0 amt *> opQuery uniC)

chargeCustomer :: CustomerId -> [OrderLine] -> TpccOp Int
chargeCustomer cid ols =
  let calcO :: TpccOp Int
      calcO = mkOp uniC idC $ \s -> (idE, olTotal s ols)
      addO = edLiftP (tpccCustomersEd *: keyEd cid *: cBalanceEd) preAdd
  in calcO <*>= addO

insertOrder :: ReplicaId -> Order -> TpccOp OrderId
insertOrder rid o = edLift tpccOrdersEd $ mkOp
  insertAny
  -- The following write-requirement (insertAny) is stronger than
  -- necessary, but MapC doesn't give a way to "insert using any key
  -- which is a pair where the first component matches a particular
  -- value."  One solution to this issue is to use a map-of-maps
  -- instead of a pair-keyed map.
  insertAny
  (\s -> let oid = newOid s rid
         in (insertE oid o, oid))

newOid :: Map OrderId Order -> ReplicaId -> OrderId
newOid s rid =
  let nums = map snd . filter ((== rid) . fst) $ Map.keys s
  in case nums of
       [] -> (rid,0)
       ns -> (rid, maximum ns + 1)

takeItems :: WarehouseId -> [OrderLine] -> TpccOp ()
takeItems w ols = traverseAA_ itemOp (itemReqs w ols)

traverseAA_
  :: (Applicative f, Applicative g)
  => (a -> f (g b))
  -> [a]
  -> f (g ())
traverseAA_ f = foldr (\a m -> f a *>*> m) (pure (pure ()))

itemReqs :: WarehouseId -> [OrderLine] -> [((WarehouseId, ItemId), Int)]
itemReqs w ols = Map.toList $ foldl
  (\m ol -> Map.alter (g ol) (w, ol^.olItemId) m)
  Map.empty
  ols
  where g ol a = Just $ ol^.olQuantity + (case a of
                                            Just n -> n
                                            Nothing -> 0)

itemOp :: ((WarehouseId, ItemId), Int) -> TpccOp Int
itemOp (i,n) = edLift
  (tpccStockEd *: keyEd i *: sQuantityEd)
  (subGuard 0 n)

olTotal :: Tpcc -> [OrderLine] -> Int
olTotal s = foldl
  (\a ol -> case olCost s ol of
              Just c -> a + c)
  0

olCost :: Tpcc -> OrderLine -> Maybe Int
olCost s ol = do
  item <- s ^. tpccItems . at (ol^.olItemId)
  return $ ol^.olQuantity * item^.iPrice
