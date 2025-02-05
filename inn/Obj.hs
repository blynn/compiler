module Obj where

import Ast
import Base
import Charser
import Map

everLE n = chr r : everLE q where (q, r) = divMod n 256
biggie k = reverse . take k . everLE
unbiggie = foldl (\acc b -> 256*acc + b) 0

cborTC ty n
  | n < 24    = hdr n
  | n < 2^8   = hdr 24 . ch n
  | n < 2^16  = hdr 25 . (biggie 2 n++)
  | n < 2^32  = hdr 26 . (biggie 4 n++)
  | otherwise = hdr 27 . (biggie 8 n++)
  where
  hdr k = ch $ ty*32 + k
  ch n = (chr n:)

data CBOR
  = CBORZ Int
  | CBORBlob [Int]
  | CBORText String
  | CBORArray [CBOR]
  | CBORMap [(CBOR, CBOR)]
  -- | CBORTag Int CBOR

cborLookup k = asum . map go where
  go (CBORText s, v) | k == s = Just v
  go _ = Nothing

cborEncode = \case
  CBORZ n
    | n >= 0 -> cborTC 0 n
    | otherwise -> cborTC 1 $ 0 - 1 - n
  CBORBlob bs -> cborTC 2 (length bs) . (++) (chr <$> bs)
  CBORText  s -> cborTC 3 (length s) . (++) s
  CBORArray a -> cborTC 4 (length a) . foldr (.) id (cborEncode <$> a)
  CBORMap   m -> cborTC 5 (length m) . foldr (.) id (map (\(k, v) -> cborEncode k . cborEncode v) m)
  -- CBORTag n c -> cborTC 6 n . cborEncode c

infixr 3 ***
f *** g = \(x, y) -> (f x, g y)

cborType = \case
  TC s -> CBORText $ 'C':s
  TV s -> CBORText $ 'V':s
  TAp x y -> CBORArray [cborType x, cborType y]

cborPred (Pred s t) = CBORArray [CBORText s, cborType t]

cborInstance (Instance ty name ctx _) = CBORArray [cborType ty, CBORText name, CBORArray $ cborPred <$> ctx]

cborQual (Qual ps t) = CBORArray $ cborType t : map cborPred ps

cborConstr (Constr s sts) = CBORArray [CBORText s, CBORMap $ (CBORText *** cborType) <$> sts]

cborFixity (n, a) = CBORArray [CBORZ n, CBORZ case a of
  NAssoc -> 0
  LAssoc -> 1
  RAssoc -> 2
  ]

cborObj (Module neat syms mem) = CBORArray
  [ CBORArray $ (\(k, v) -> CBORArray [CBORText k, cborQual $ fst v, go $ syms ! k]) <$> toAscList (typedAsts neat)
  , CBORArray $ go <$> mem
  , cborNeat neat
  ]
  where
  go = \case
    Left (m, s) -> CBORArray [CBORText m, CBORText s]
    Right n -> CBORZ n

cborNeat neat = CBORMap $ mayExport
  [ (CBORText "foreign", CBORMap $ map (CBORText *** cborType) $ toAscList $ ffiImports neat)
  , (CBORText "class", CBORMap $ map (CBORText *** CBORArray . map CBORText) $ toAscList $ typeclasses neat)
  , (CBORText "instance", CBORMap $ map (CBORText *** CBORArray . map cborInstance) $ toAscList $ instances neat)
  , (CBORText "data", CBORMap $ map (CBORText *** \(q, cs) -> CBORArray (cborQual q:map cborConstr cs)) $ toAscList $ dataCons neat)
  , (CBORText "infix", CBORMap $ map (CBORText *** cborFixity) $ toAscList $ opFixity neat)
  , (CBORText "alias", CBORMap $ map (CBORText *** \(vs, t) -> CBORArray [CBORArray (CBORText <$> vs), cborType t]) $ toAscList $ typeAliases neat)
  ]
  where
  mayExport = case moduleExports $ exportStuff neat of
    Nothing -> id
    Just xs -> ((CBORText "export", CBORArray $ CBORText <$> xs):)

hex n = chr $ n + (if n <= 9 then ord '0' else ord 'a' - 10)
xx c = (hex q:) . (hex r:) where (q, r) = divMod (ord c) 16

scriptify (name, obj) = foldr (.) id $ (. ('\n':)) <$>
  [ ("cat > "++) . ((if name == "#" then "prim" else name)++) . (".ob << EOF"++)
  , ("module "++) . (name++) . (" [x|"++)
  , foldr (.) id $ xx <$> (cborEncode (cborObj obj) "")
  , ("|]"++)
  , ("EOF"++)
  ]

toCsource m = (++) "unsigned char objmapraw[] = {\n"
  . foldr (.) id ((("0x"++) .) . (. (',':)) . xx <$> bin)
  . (++) [r|
};
u objmapbytes() {
  u r = _K, i = sizeof(objmapraw)/sizeof(*objmapraw) - 1;
  do { r = app(app(_CONS, app(_NUM, objmapraw[i])), r); } while (i--);
  return r;
}
|]
  where
  bin = cborEncode (CBORMap $ (CBORText *** cborObj) <$> toAscList m) ""

anyChar = sat $ const True

cborParser = do
  (ty, r) <- (`divMod` 32) . ord <$> anyChar
  when (r > 27) $ error "bad count"
  n <- if r < 24 then pure r else unbiggie . map ord <$> replicateM (2^(r - 24)) anyChar
  case ty of
    0 -> pure $ CBORZ n
    1 -> pure $ CBORZ $ 0 - n
    2 -> CBORBlob . map ord <$> replicateM n anyChar
    3 -> CBORText <$> replicateM n anyChar
    4 -> CBORArray <$> replicateM n cborParser
    5 -> CBORMap <$> replicateM n (liftA2 (,) cborParser cborParser)
    -- 6 -> CBORTag n <$> cborParser
    _ -> error "unsupported type"

must f k m = maybe undefined f $ cborLookup k m

uncborObj (CBORArray [CBORArray ds, CBORArray mem, CBORMap ne]) = Module
  (uncborNeat ds ne)
  (fromList $ (\(CBORArray [CBORText k, _, x]) -> (k, go x)) <$> ds)
  (go <$> mem)
  where
  go = \case
    CBORArray [CBORText m, CBORText s] -> Left (m, s)
    CBORZ n -> Right n

decodeObject xs = uncborObj cbor where
  Right (cbor, "") = unCharser cborParser $ chr <$> xs

decodeObjectMap xs = fromList $ (\(CBORText k, v) -> (k, uncborObj v)) <$> kvs
  where
  Right (CBORMap kvs, "") = unCharser cborParser $ chr <$> xs

uncborText (CBORText s) = s

uncborNeat ds m = neatEmpty
  { typeclasses = must (\(CBORMap cs) -> fromList $
    (\(CBORText k, CBORArray as) -> (k, uncborText <$> as)) <$> cs) "class" m
  , instances = must (\(CBORMap is) -> fromList $
    (\(CBORText k, CBORArray as) -> (k, uncborInstance <$> as)) <$> is) "instance" m
  , typedAsts = fromList $ (\(CBORArray [CBORText k, q, _]) -> (k, (uncborQual q, V "_"))) <$> ds
  , dataCons = must (\(CBORMap ds) -> fromList $
    (\(CBORText k, CBORArray (q:cs)) -> (k, (uncborQual q, uncborConstr <$> cs))) <$> ds) "data" m
  , opFixity = must (\(CBORMap os) -> fromList $
    (\(CBORText k, CBORArray [CBORZ n, CBORZ a]) -> (k, (n, toFixity a))) <$> os) "infix" m
  , typeAliases = must (\(CBORMap ts) -> fromList $
    (\(CBORText k, CBORArray[CBORArray vs, ty]) -> (k, (uncborText <$> vs, uncborType ty))) <$> ts) "alias" m
  , exportStuff = (exportStuff neatEmpty) {
      moduleExports = case cborLookup "export" m of
        Nothing -> Nothing
        Just (CBORArray xs) -> Just $ uncborText <$> xs
    }
  , ffiImports = must (\(CBORMap is) -> fromList $
    (\(CBORText k, ty) -> (k, uncborType ty)) <$> is) "foreign" m
  }
  where
  uncborClass (CBORText k, CBORArray as) = (k, uncborText <$> as)
  uncborInstance (CBORArray [ty, CBORText name, CBORArray ps]) =
    Instance (uncborType ty) name (uncborPred <$> ps) Tip

uncborPred (CBORArray [CBORText s, ty]) = Pred s $ uncborType ty

uncborType = \case
  CBORText (h:s) -> (if h == 'C' then TC else TV) s
  CBORArray [x, y] -> TAp (uncborType x) (uncborType y)

uncborQual (CBORArray (t:ps)) = Qual (uncborPred <$> ps) $ uncborType t

uncborConstr (CBORArray [CBORText s, CBORMap m]) = Constr s $ (uncborText *** uncborType) <$> m

toFixity = \case
  0 -> NAssoc
  1 -> LAssoc
  2 -> RAssoc
