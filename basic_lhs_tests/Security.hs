data User = Chair | Alice | Bob | Default

-- | Value tagged with a policy  

{-@ data Tagged a <p :: User->Prop> = Tagged (content :: a) @-}
data Tagged a = Tagged a

{-@ ret :: forall a <p :: User -> Prop>.
              a -> Tagged <p> a @-}
ret :: a -> Tagged a
ret x = Tagged x 


{-@ bind :: forall a b <p :: User -> Prop, f:: a -> b -> Prop>.
                x: Tagged <p> a
                -> (u:a -> Tagged <p> (b <f u>))
                -> Tagged <p> (b<f (content x)>)
@-}
bind :: Tagged a -> (a -> Tagged b) -> (Tagged b)
bind (Tagged x) f = f x  

{-@ liftM :: forall a b <p :: User -> Prop, f:: a -> b -> Prop>.
                x: Tagged <p> a
                -> (u:a -> b<f u>)
                -> Tagged <p> (b<f (content x)>)
@-}
liftM :: Tagged a -> (a -> b) -> Tagged b
liftM x f = bind x (\x' -> ret (f x'))

--{-@ lift :: forall a b <p :: User -> Prop, f:: a -> b -> Prop>.
--                (u:a -> b<f u>)
--                -> x: Tagged <p> a
--                -> Tagged <p> (b<f (content x)>)
-- @-}
--lift :: (a -> b) -> Tagged a -> Tagged b
--lift f x = bind x (\x' -> ret (f x'))

--liftM2 :: (a -> b -> c) -> Tagged a -> Tagged b -> Tagged c
--liftM2 f x y = bind x (\x' -> bind y (\y' -> ret (f x' y')))  

data PaperId
data World

{- Sensitive data and policies -}
   
-- | Current session user
{-@ measure sessionUser :: World -> User @-}
sessionUser = Alice
{-@ assume getSessionUser :: w:World -> {u:User | u == sessionUser w} @-}
getSessionUser :: World -> User
getSessionUser w = Alice

-- | PC chair (public)
{-@ measure chair :: World -> User @-}
chair = Chair
{-@ assume getChair :: w:World -> {u:User | chair w == u } @-}
getChair :: World -> User
getChair w = Chair

-- | Paper title (public)
getPaperTitle :: World -> PaperId -> String
getPaperTitle w pid = "Waow" -- hack for now

-- | Paper authors (visible only to the chair)
{-@ assume getPaperAuthors :: w: World -> pid : PaperId -> {l: [User] | sessionUser w == chair w} @-}
getPaperAuthors :: World -> PaperId -> [User]
getPaperAuthors w pid = [Alice, Bob] -- hack for now
defaultPaperAuthors = [Default]

{- Client code -}
{-}  
-- | Show paper info to session user
-- | Repair: check that u is chair
showPaper :: w: World -> pid: PaperId -> World
showPaper = \w . \pid .
  let u = getSessionUser w in
  let title = getPaperTitle w pid in
  let authors = liftM show (getPaperAuthors w pid) in
  let out = liftM2 strcat title authors in
  print w u out -}