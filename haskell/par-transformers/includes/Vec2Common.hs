
-- NOT a module... this is included into other files:
--------------------------------------------------------------------------------

type ParVec2T s1 e1 e2 p e s a =
     ParST (STTup2 (FLIPTY e1) (FLIPTY e2) s1) p e s a

-- | Restricted version of `runParST` which initialized the state with a single,
-- boxed vector of a given size.  All elements start uninitialized.
{-# INLINE runParVec2T #-}
runParVec2T :: forall e1 e2 p e s a .
               (CONSTRAINT(e1)) => (CONSTRAINT(e2)) => (ParThreadSafe p)
             => (Int, Int)
             -> (forall s1 . ParVec2T s1 e1 e2 p e s a)
             -> p e s a
runParVec2T (size1, size2) comp =
  runParST (error "runParVec -- this initial value should be unused.") $ do
    vec1 <- liftST $ MU.new size1
    vec2 <- liftST $ MU.new size2
    S.put (STTup2 (FLPIT vec1) (FLPIT vec2))
    comp

-- | Extract a pointer to the whole Vector in its normal, usable @STVector@ form.
--   Use the `liftST` operator to act on it.
{-# INLINE reify #-}
reify :: ParThreadSafe p => ParVec2T s1 e1 e2 p e s (MU.MVector s1 e1, MU.MVector s1 e2)
reify = do
  STTup2 (FLPIT vec1) (FLPIT vec2) <- S.get
  return (vec1, vec2)

--------------------------------------------------------------------------------

-- | Swap the two state vectors.
{-# INLINE swapState #-}
swapState :: ParThreadSafe p => ParVec2T s1 a a p e s ()
swapState = do
  STTup2 (FLPIT vecL) (FLPIT vecR) <- S.get
  S.put $ STTup2 (FLPIT vecR) (FLPIT vecL)

-- | Write to the (implicit) left vector state.
{-# INLINE writeL #-}
writeL :: ParThreadSafe p => Int -> eL -> ParVec2T s eL rR p e s ()
writeL ind val = do
  STTup2 (FLPIT vecL) _ <- S.get
  liftST $ MU.write vecL ind val

-- | Read the (implicit) left vector state.
{-# INLINE readL #-}
readL :: ParThreadSafe p => Int -> ParVec2T s eL eR p e s eL
readL ind = do
  STTup2 (FLPIT vecL) _ <- S.get
  liftST $ MU.read vecL ind

-- | Return the length of the (implicit) left vector state.
{-# INLINE lengthL #-}
lengthL :: ParThreadSafe p => ParVec2T s1 eL eR p e s Int
lengthL = do
  STTup2 (FLPIT vecL) _ <- S.get
  return $ MU.length vecL

-- | Update the left vector state by swapping two elements.
{-# INLINE swapL #-}
swapL :: ParThreadSafe p => Int -> Int -> ParVec2T s1 eL eR p e s ()
swapL x y = do
  STTup2 (FLPIT vecL) _ <- S.get
  liftST $ MU.swap vecL x y

-- | Update the left vector state by dropping the first @n@ elements.
{-# INLINE dropL #-}
dropL :: ParThreadSafe p => Int -> ParVec2T s1 eL eR p e s ()
dropL n = do
  STTup2 (FLPIT vecL) vecR <- S.get
  S.put $ STTup2 (FLPIT (MU.drop n vecL)) vecR

-- | Update the left vector state by taking the first @n@ elements, discarding the rest.
{-# INLINE takeL #-}
takeL :: ParThreadSafe p => Int -> ParVec2T s1 eL eR p e s ()
takeL n = do
  STTup2 (FLPIT vecL) vecR <- S.get
  S.put $ STTup2 (FLPIT (MU.take n vecL)) vecR

-- | Destructively replace the left vector with a bigger vector,
-- adding the given number of elements.  The new elements are
-- uninitialized and will result in errors if read.
{-# INLINE growL #-}
growL :: ParThreadSafe p => Int -> ParVec2T s1 eL eR p e s ()
growL n = do
  STTup2 (FLPIT vecL) vecR <- S.get
  vecL2 <- liftST $ MU.grow vecL n
  S.put $ STTup2 (FLPIT vecL2) vecR

-- | Mutate all the elements of the left vector, setting them to the
-- given value.
{-# INLINE setL #-}
setL :: ParThreadSafe p => eL -> ParVec2T s1 eL eR p e s ()
setL val = do
  STTup2 (FLPIT vecL) _ <- S.get
  liftST $ MU.set vecL val

-- Helpers for the other vector in the state.

-- | Write to the (implicit) right vector state.
{-# INLINE writeR #-}
writeR :: ParThreadSafe p => Int -> eR -> ParVec2T s1 eL eR p e s ()
writeR ind val = do
  STTup2 _ (FLPIT vecR) <- S.get
  liftST $ MU.write vecR ind val

-- | Read the (implicit) right vector state.
{-# INLINE readR #-}
readR :: ParThreadSafe p => Int -> ParVec2T s1 eL eR p e s eR
readR ind = do
  STTup2 _ (FLPIT vecR) <- S.get
  liftST $ MU.read vecR ind

-- | Return the length of the (implicit) right vector state.
{-# INLINE lengthR #-}
lengthR :: ParThreadSafe p => ParVec2T s1 eL eR p e s Int
lengthR = do
  STTup2 _ (FLPIT vecR) <- S.get
  return $ MU.length vecR

-- | Update the vector state by swapping two elements.
{-# INLINE swapR #-}
swapR :: ParThreadSafe p => Int -> Int -> ParVec2T s1 eL eR p e s ()
swapR x y = do
  STTup2 _ (FLPIT vecR) <- S.get
  liftST $ MU.swap vecR x y

-- | Update the right vector state by dropping the first @n@ elements.
{-# INLINE dropR #-}
dropR :: ParThreadSafe p => Int -> ParVec2T s1 eL eR p e s ()
dropR n = do
  STTup2 vecL (FLPIT vecR) <- S.get
  S.put $ STTup2 vecL (FLPIT (MU.drop n vecR))

-- | Update the right vector state by taking the first @n@ elements,
-- discarding the rest.
{-# INLINE takeR #-}
takeR :: ParThreadSafe p => Int -> ParVec2T s1 eL eR p e s ()
takeR n = do
  STTup2 vecL (FLPIT vecR) <- S.get
  S.put $ STTup2 vecL (FLPIT (MU.take n vecR))

-- | Destructively replace the right vector with a bigger vector,
-- adding the given number of elements.  The new elements are
-- uninitialized and will result in errors if read.
{-# INLINE growR #-}
growR :: ParThreadSafe p => Int -> ParVec2T s1 eL eR p e s ()
growR n = do
  STTup2 vecL (FLPIT vecR) <- S.get
  vecR2 <- liftST $ MU.grow vecR n
  S.put $ STTup2 vecL (FLPIT vecR2)

-- | Mutate all the elements of the right vector, setting them to the
-- given value.
{-# INLINE setR #-}
setR :: ParThreadSafe p => eR -> ParVec2T s1 eL eR p e s ()
setR val = do
  STTup2 _ (FLPIT vecR) <- S.get
  liftST $ MU.set vecR val
