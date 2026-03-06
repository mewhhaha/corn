{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Engine.Corn
  ( Kernel
  , Graph
  , Build
  , Routine
  , ProgramM
  , Local
  , Handle
  , Pass
  , Batch
  , Wait
  , Sticky
  , Events
  , DTime
  , Patch
  , EntityPatch
  , Query
  , kernel
  , graph
  , spawn
  , program
  , runFrame
  , run
  , event
  , done
  , waitSticky
  , sticky
  , flush
  , pass
  , await
  , query
  , collect
  , each
  , for
  , eachM
  , spawnEach
  , send
  , dt
  , time
  , sample
  , step
  , world
  , edit
  , patch
  , emptyPatch
  , drive
  , set
  , update
  , del
  , at
  , put
  , relate
  , unrelate
  ) where

import Prelude

import Data.Kind (Type)
import GHC.Stack (HasCallStack)
import Data.Typeable (Typeable)
import qualified Engine.Data.ECS as E
import qualified Engine.Data.FRP as F
import qualified Engine.Data.Program as P

type Kernel c msg = P.Graph c msg
type Graph c msg = Kernel c msg
type Build c msg = P.GraphM c msg
type Routine c msg = P.ProgramM c msg
type ProgramM c msg = Routine c msg
type Local c msg = P.EntityM c msg
type Handle a = P.Handle a
type Pass c msg a = P.Batch c msg a
type Batch c msg a = Pass c msg a
type Events a = F.Events a
type DTime = F.DTime
type Patch c = P.Patch c
type EntityPatch c = P.EntityPatch c
type Query c a = E.Query c a

data Scope
  = KernelScope
  | LocalScope

newtype Sticky msg a = Sticky (P.Sticky msg a)

instance Functor (Sticky msg) where
  fmap f (Sticky inner) = Sticky (fmap f inner)

instance Applicative (Sticky msg) where
  pure = Sticky . pure
  Sticky f <*> Sticky a = Sticky (f <*> a)

data Wait scope c msg a where
  WaitEvent :: (msg -> Bool) -> Wait scope c msg (Events msg)
  WaitDone :: Handle a -> Wait scope c msg a
  WaitSticky :: Sticky msg a -> Wait scope c msg a
  WaitFlush :: Wait 'KernelScope c msg ()
  WaitPass :: Pass c msg a -> Wait 'KernelScope c msg a

kernel :: Build c msg () -> Kernel c msg
kernel = P.graph

graph :: Build c msg () -> Graph c msg
graph = kernel

spawn :: Routine c msg a -> Build c msg (Handle a)
spawn = P.program

program :: Routine c msg a -> Build c msg (Handle a)
program = spawn

runFrame :: DTime -> E.World c -> Events msg -> Kernel c msg -> (E.World c, Events msg, Kernel c msg)
runFrame = P.run

run :: DTime -> E.World c -> Events msg -> Graph c msg -> (E.World c, Events msg, Graph c msg)
run = runFrame

event :: (msg -> Bool) -> Wait scope c msg (Events msg)
event = WaitEvent

done :: Handle a -> Wait scope c msg a
done = WaitDone

waitSticky :: Sticky msg a -> Wait scope c msg a
waitSticky = WaitSticky

sticky :: (msg -> Maybe a) -> Sticky msg a
sticky = Sticky . P.sticky

flush :: forall c msg. Wait 'KernelScope c msg ()
flush = WaitFlush

pass :: forall c msg a. Pass c msg a -> Wait 'KernelScope c msg a
pass = WaitPass

class Awaitable m a b | m a -> b where
  await :: a -> m b

instance Awaitable (P.ProgramM c msg) (Wait 'KernelScope c msg a) a where
  await waitOn =
    case waitOn of
      WaitEvent p -> P.await p
      WaitDone h -> P.await h
      WaitSticky (Sticky s) -> P.await s
      WaitFlush -> P.awaitUpdate
      WaitPass b -> P.await b

instance Awaitable (P.ProgramM c msg) (Handle a) a where
  await = P.await

instance Awaitable (P.ProgramM c msg) (msg -> Bool) (Events msg) where
  await = P.await

instance Awaitable (P.ProgramM c msg) (Sticky msg a) a where
  await (Sticky s) = P.await s

instance Awaitable (P.ProgramM c msg) (Pass c msg a) a where
  await = P.await

instance Awaitable (P.EntityM c msg) (Wait 'LocalScope c msg a) a where
  await waitOn =
    case waitOn of
      WaitEvent p -> P.await p
      WaitDone h -> P.await h
      WaitSticky (Sticky s) -> P.await s

instance Awaitable (P.EntityM c msg) (Handle a) a where
  await = P.await

instance Awaitable (P.EntityM c msg) (msg -> Bool) (Events msg) where
  await = P.await

instance Awaitable (P.EntityM c msg) (Sticky msg a) a where
  await (Sticky s) = P.await s

query :: forall a c. E.Queryable c a => Query c a
query = E.query

collect :: forall c msg a. Query c a -> Pass c msg [(E.Entity, a)]
collect = P.collect

each :: forall a c msg. E.Queryable c a => (a -> EntityPatch c) -> Pass c msg ()
each = P.each

for :: forall c msg a. Query c a -> (a -> EntityPatch c) -> Pass c msg ()
for = P.eachQ

eachM ::
  forall a c msg.
  (HasCallStack, Typeable a, E.Queryable c a) =>
  (a -> Local c msg ()) ->
  Pass c msg ()
eachM = P.eachM

spawnEach ::
  forall a c msg.
  (HasCallStack, Typeable a) =>
  Query c a ->
  (a -> Local c msg ()) ->
  Pass c msg ()
spawnEach = P.eachMQ

send :: P.MonadProgram c msg m => Events msg -> m ()
send = P.send

dt :: P.MonadProgram c msg m => m DTime
dt = P.dt

time :: P.MonadProgram c msg m => m F.Time
time = P.time

sample :: P.MonadProgram c msg m => F.Tween a -> m a
sample = P.sample

step ::
  (HasCallStack, P.MonadProgram c msg m, Typeable a, Typeable b) =>
  F.Step a b ->
  a ->
  m b
step = P.step

world :: Patch c -> Routine c msg ()
world = P.world

edit :: EntityPatch c -> Local c msg ()
edit = P.edit

patch :: (E.World c -> E.World c) -> Patch c
patch = P.patch

emptyPatch :: Patch c
emptyPatch = P.emptyPatch

drive :: forall a c. (E.Component c a, E.ComponentBit c a) => E.Entity -> F.Step () a -> Patch c
drive = P.drive

set :: forall a c. (E.Component c a, E.ComponentBit c a) => a -> EntityPatch c
set = P.set

update :: forall a c. (E.Component c a, E.ComponentBit c a) => (a -> a) -> EntityPatch c
update = P.update

del :: forall a c. (E.Component c a, E.ComponentBit c a) => EntityPatch c
del = P.del @a

at :: E.Entity -> EntityPatch c -> Patch c
at = P.at

put :: Typeable a => a -> Patch c
put = P.put

relate :: forall (r :: Type) c. Typeable r => E.Entity -> E.Entity -> Patch c
relate = P.relate @r

unrelate :: forall (r :: Type) c. Typeable r => E.Entity -> E.Entity -> Patch c
unrelate = P.unrelate @r
