{-# LANGUAGE TemplateHaskell #-}

module UCap.TPCC.Data where

import Data.Map (Map)
import Lens.Micro.Platform
import UCap.Domain
import UCap.Lifter

type CustomerId = String

type ItemId = String

type ReplicaId = Int

type OrderId = (ReplicaId, Int)

type CarrierId = String

type WarehouseId = String

data OrderLine
  = OrderLine { _olItemId :: ItemId
              , _olQuantity :: Int
              , _olAmount :: Int
              }
  deriving (Show,Read,Eq,Ord)

makeLenses ''OrderLine

data OrderInfo
  = OrderInfo { _oiCId :: CustomerId
              , _oiDate :: String
              , _oiLines :: [OrderLine]
              }
  deriving (Show,Read,Eq,Ord)

makeLenses ''OrderInfo

type Order = (OrderInfo, Either () CarrierId)

mkOrder :: CustomerId -> String -> [OrderLine] -> Order
mkOrder a b c = (OrderInfo a b c, Left ())

oCId :: Lens' Order CustomerId
oCId = _1 . oiCId

oDate :: Lens' Order String
oDate = _1 . oiDate

oLines :: Lens' Order [OrderLine]
oLines = _1 . oiLines

oCarrierId :: Lens' (a,b) b
oCarrierId = _2

type OrderC
  = (IdentityC OrderInfo, EitherC' (IdentityC ()) (IdentityC CarrierId))

type OrderE = CEffect OrderC

setCarrierId :: CarrierId -> OrderE
setCarrierId i = idE & oCarrierId .~ (SetR i)

data Item
  = Item { _iName :: String
         , _iPrice :: Int
         }
  deriving (Show,Read,Eq,Ord)

makeLenses ''Item

type Stock = (Int,Int,Int,Int)

sQuantity :: Lens' (a,b,c,d) a
sQuantity = _1

quantityLf :: Lifter StockC IntC
quantityLf = _1ed

-- sQuantityEd = Editor
--   (pure . (^. sQuantity))
--   (meetTo sQuantity)
--   (plusTo sQuantity)
--   (plusTo sQuantity)

sYtd :: Lens' (a,b,c,d) b
sYtd = _2

sOrderCount :: Lens' (a,b,c,d) c
sOrderCount = _3

sRemoteCount :: Lens' (a,b,c,d) d
sRemoteCount = _4

type IntE = CounterE Int
type IntC = CounterC Int

type StockE = (IntE, IntE, IntE, IntE)

type StockC = (IntC, IntC, IntC, IntC)

data CustomerInfo
  = CustomerInfo { _ciName :: String }
  deriving (Show,Read,Eq,Ord)

makeLenses ''CustomerInfo

type Customer = (CustomerInfo, Int)

type CustomerE = (IdentityE CustomerInfo, IntE)

type CustomerC = (IdentityC CustomerInfo, IntC)

cName :: Lens' Customer String
cName = _1 . ciName

cBalance :: Lens' (a,b) b
cBalance = _2

balanceLf :: Lifter CustomerC IntC
balanceLf = _2ed

type Tpcc
  = ( Map (WarehouseId, ItemId) Stock -- stock
    , Map ItemId Item -- items
    , Map CustomerId Customer -- customers
    , Map OrderId Order -- orders
    )

type TpccE
  = ( MapE (WarehouseId, ItemId) StockE
    , MapE ItemId (IdentityE Item)
    , MapE CustomerId CustomerE
    , MapE OrderId OrderE
    )

type StocksC = MapC' (WarehouseId, ItemId) StockC

type ItemsC = MapC' ItemId (IdentityC Item)

type CustomersC = MapC' CustomerId CustomerC

type OrdersC = MapC' OrderId OrderC

type TpccC
  = ( MapC' (WarehouseId, ItemId) StockC
    , MapC' ItemId (IdentityC Item)
    , MapC' CustomerId CustomerC
    , MapC' OrderId OrderC
    )

tpccStock :: Lens' (a,b,c,d) a
tpccStock = _1

stockLf :: Lifter TpccC (MapC' (WarehouseId, ItemId) StockC)
stockLf = _1ed

tpccItems :: Lens' (a,b,c,d) b
tpccItems = _2

itemsLf :: Lifter TpccC (MapC' ItemId (IdentityC Item))
itemsLf = _2ed

tpccCustomers :: Lens' (a,b,c,d) c
tpccCustomers = _3

customersLf :: Lifter TpccC (MapC' CustomerId CustomerC)
customersLf = _3ed

tpccOrders :: Lens' (a,b,c,d) d
tpccOrders = _4

ordersLf :: Lifter TpccC (MapC' OrderId OrderC)
ordersLf = _4ed
