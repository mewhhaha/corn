{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module ECSProps
  ( prop_spawn_get
  , prop_set_get
  , prop_query_superset
  , prop_put_getr
  , prop_query_app
  , prop_query_alt
  , prop_query_pruning_applicative
  , prop_query_pruning_monad
  , prop_query_pruning_alternative
  , prop_query_pruning_sum
  , prop_require_query_pruning_passthrough
  , prop_query_queryable
  , prop_query_queryable_sum
  , prop_bag_mask_value_coherence
  , prop_bag_apply_edit_packed_non_structural
  , prop_bag_apply_edit_packed_structural
  , prop_run_query_sig_gate
  , prop_relations
  , prop_parent_child
  , prop_transform_inverse
  ) where

import Control.Applicative ((<|>))
import Data.Bits (bit, complement, (.&.))
import Data.Maybe (isJust, isNothing)
import qualified Engine.Data.ECS as E
import GHC.Generics (Generic)
import qualified Engine.Data.Transform as T

data C
  = CInt Int
  | CBool Bool
  | CLocal T.Local
  | CGlobal T.Global
  deriving (Generic)

instance E.ComponentId C

type World = E.World C

prop_spawn_get :: Int -> Bool
prop_spawn_get x =
  let (e, w) = E.spawn x (E.emptyWorld :: World)
  in E.get e w == Just x

prop_set_get :: Int -> Bool
prop_set_get x =
  let (e, w) = E.spawn () (E.emptyWorld :: World)
      w' = E.set e x w
  in E.get e w' == Just x

prop_query_superset :: Int -> Bool
prop_query_superset x =
  let (e, w) = E.spawn (x, True) (E.emptyWorld :: World)
      results = E.runq (E.comp :: E.Query C Int) w
  in (e, x) `elem` results

prop_put_getr :: Int -> Bool
prop_put_getr x =
  let w = E.put x (E.emptyWorld :: World)
  in E.getr w == Just x

prop_query_app :: Int -> Bool
prop_query_app x =
  let (e, w) = E.spawn (x, True) (E.emptyWorld :: World)
      q = (,) <$> (E.comp :: E.Query C Int) <*> (E.comp :: E.Query C Bool)
  in E.runq q w == [(e, (x, True))]

prop_query_alt :: Int -> Bool
prop_query_alt x =
  let (e, w) = E.spawn x (E.emptyWorld :: World)
      q = (E.comp :: E.Query C Int) <|> fmap (const 0) (E.comp :: E.Query C Bool)
  in E.runq q w == [(e, x)]

prop_query_pruning_applicative :: Int -> Bool
prop_query_pruning_applicative x =
  let (_, w) = E.spawn (x, True) (E.emptyWorld :: World)
      q = (,) <$> (E.comp :: E.Query C Int) <*> (E.comp :: E.Query C Bool)
  in E.queryHasSigPruning q && not (null (E.runq q w))

prop_query_pruning_monad :: Int -> Bool
prop_query_pruning_monad x =
  let (_, w) = E.spawn (x, True) (E.emptyWorld :: World)
      q = do
        n <- (E.comp :: E.Query C Int)
        b <- (E.comp :: E.Query C Bool)
        pure (n, b)
  in not (E.queryHasSigPruning q) && not (null (E.runq q w))

prop_query_pruning_alternative :: Int -> Bool
prop_query_pruning_alternative x =
  let (_, w) = E.spawn x (E.emptyWorld :: World)
      q = (E.comp :: E.Query C Int) <|> fmap (const 0) (E.comp :: E.Query C Bool)
  in not (E.queryHasSigPruning q) && not (null (E.runq q w))

prop_query_pruning_sum :: Int -> Bool
prop_query_pruning_sum x =
  let (_, w) = E.spawn x (E.emptyWorld :: World)
      q = E.querySum @QSum
  in not (E.queryHasSigPruning q) && not (null (E.runq q w))

prop_require_query_pruning_passthrough :: Int -> Bool
prop_require_query_pruning_passthrough x =
  let (e, w) = E.spawn (x, True) (E.emptyWorld :: World)
      q = (,) <$> (E.comp :: E.Query C Int) <*> (E.comp :: E.Query C Bool)
  in E.runq (E.requireQuerySigPruning "ecsprops/applicative" q) w == [(e, (x, True))]

data QB = QB
  { qbInt :: Int
  , qbMaybe :: Maybe Bool
  } deriving (Eq, Show, Generic)

prop_query_queryable :: Int -> Bool -> Bool
prop_query_queryable x b =
  let (e, w) = E.spawn (x, b) (E.emptyWorld :: World)
  in E.runq (E.query @QB) w == [(e, QB x (Just b))]

data QSum = QInt Int | QBool Bool
  deriving (Eq, Show, Generic)

instance E.QueryableSum C QSum

prop_query_queryable_sum :: Int -> Bool
prop_query_queryable_sum x =
  let (e, w) = E.spawn x (E.emptyWorld :: World)
  in E.runq (E.querySum @QSum) w == [(e, QInt x)]

prop_bag_mask_value_coherence :: Int -> Bool -> Int -> Bool -> Bool -> Bool
prop_bag_mask_value_coherence x b dx setLocal delBool =
  let (_, w0) = E.spawn (x, b) (E.emptyWorld :: World)
      bag0 =
        case E.entityRows w0 of
          E.EntityRow _ _ bag : _ -> bag
          [] -> error "expected spawned entity"
      local = T.Local (T.translate (fromIntegral x, fromIntegral dx, 0))
      edit0 = E.bagEditUpdate @C @Int (+ dx)
      edit1 =
        if delBool
          then edit0 <> E.bagEditDel @C @Bool
          else edit0 <> E.bagEditSet @C @Bool (not b)
      edit2 =
        if setLocal
          then edit1 <> E.bagEditSet @C @T.Local local
          else edit1
      bag1 = E.bagApplyEditPacked edit2 bag0
      sig1 = E.sigFromBag bag1
      coherentInt =
        let hasBit = (sig1 .&. bit (E.componentBitOf @C @Int)) /= 0
            hasVal = isJust (E.bagGet @C @Int bag1)
        in hasBit == hasVal
      coherentBool =
        let hasBit = (sig1 .&. bit (E.componentBitOf @C @Bool)) /= 0
            hasVal = isJust (E.bagGet @C @Bool bag1)
        in hasBit == hasVal
      coherentLocal =
        let hasBit = (sig1 .&. bit (E.componentBitOf @C @T.Local)) /= 0
            hasVal = isJust (E.bagGet @C @T.Local bag1)
        in hasBit == hasVal
      coherentGlobal =
        let hasBit = (sig1 .&. bit (E.componentBitOf @C @T.Global)) /= 0
            hasVal = isJust (E.bagGet @C @T.Global bag1)
        in hasBit == hasVal
  in coherentInt && coherentBool && coherentLocal && coherentGlobal

prop_bag_apply_edit_packed_non_structural :: Int -> Bool -> Int -> Int -> Bool
prop_bag_apply_edit_packed_non_structural x b dx dy =
  let (_, w0) = E.spawn (x, b) (E.emptyWorld :: World)
      bag0 =
        case E.entityRows w0 of
          E.EntityRow _ _ bag : _ -> bag
          [] -> error "expected spawned entity"
      edit2 =
        E.bagEditUpdate @C @Int (+ dx)
          <> E.bagEditSet @C @Bool (not b)
      editN = edit2 <> E.bagEditUpdate @C @Int (+ dy)
      sameResult edit =
        let bagA = E.bagApplyEdit edit bag0
            bagB = E.bagApplyEditPacked edit bag0
        in E.sigFromBag bagA == E.sigFromBag bagB
            && E.bagGet @C @Int bagA == E.bagGet @C @Int bagB
            && E.bagGet @C @Bool bagA == E.bagGet @C @Bool bagB
            && E.bagGet @C @T.Local bagA == E.bagGet @C @T.Local bagB
            && E.bagGet @C @T.Global bagA == E.bagGet @C @T.Global bagB
  in sameResult edit2 && sameResult editN

prop_bag_apply_edit_packed_structural :: Int -> Bool -> Int -> Int -> Bool
prop_bag_apply_edit_packed_structural x b dx dy =
  let (_, w0) = E.spawn (x, b) (E.emptyWorld :: World)
      bag0 =
        case E.entityRows w0 of
          E.EntityRow _ _ bag : _ -> bag
          [] -> error "expected spawned entity"
      local = T.Local (T.translate (fromIntegral dx, fromIntegral dy, 0))
      editSetMissing = E.bagEditSet @C @T.Local local
      editDelPresent = E.bagEditDel @C @Bool
      editMixed =
        editSetMissing
          <> E.bagEditUpdate @C @Int (+ dx)
          <> editDelPresent
          <> E.bagEditSet @C @Bool (not b)
      sameResult edit =
        let bagA = E.bagApplyEdit edit bag0
            bagB = E.bagApplyEditPacked edit bag0
        in E.sigFromBag bagA == E.sigFromBag bagB
            && E.bagGet @C @Int bagA == E.bagGet @C @Int bagB
            && E.bagGet @C @Bool bagA == E.bagGet @C @Bool bagB
            && E.bagGet @C @T.Local bagA == E.bagGet @C @T.Local bagB
            && E.bagGet @C @T.Global bagA == E.bagGet @C @T.Global bagB
  in sameResult editSetMissing && sameResult editDelPresent && sameResult editMixed

prop_run_query_sig_gate :: Int -> Bool -> Bool
prop_run_query_sig_gate x b =
  let (_, w0) = E.spawn (x, b) (E.emptyWorld :: World)
      qInt = E.comp :: E.Query C Int
      qPair = (,) <$> (E.comp :: E.Query C Int) <*> (E.comp :: E.Query C Bool)
  in case E.entityRows w0 of
      E.EntityRow eid' sig bag : _ ->
        let e = E.Entity eid'
            sigNoInt = sig .&. complement (bit (E.componentBitOf @C @Int))
            sigNoBool = sig .&. complement (bit (E.componentBitOf @C @Bool))
        in E.runQuerySig qInt sig e bag == Just x
            && isNothing (E.runQuerySig qInt sigNoInt e bag)
            && E.runQuerySig qPair sig e bag == Just (x, b)
            && isNothing (E.runQuerySig qPair sigNoBool e bag)
      [] -> False

data Owns

prop_relations :: Int -> Int -> Bool
prop_relations a b =
  let (e1, w1) = E.spawn a (E.emptyWorld :: World)
      (e2, w2) = E.spawn b w1
      w3 = E.relate @Owns e1 e2 w2
  in E.out @Owns e1 w3 == [e2] && E.inn @Owns e2 w3 == [e1]

prop_parent_child :: Bool
prop_parent_child =
  let (p, w1) = E.spawn (T.Local (T.translate (0,0,0))) (E.emptyWorld :: World)
      (c, w2) = E.spawn (T.Local (T.translate (2,0,0))) w1
      w3 = T.attach p c w2
      w4 = T.propagate w3
  in E.get @T.Global c w4 == Just (T.Global (T.translate (2,0,0)))

prop_transform_inverse :: Bool
prop_transform_inverse =
  let m = T.compose [T.translate (1,2,3), T.scale (2,2,2)]
  in case T.inverse m of
      Nothing -> False
      Just inv -> T.mul m inv == T.identity
