module Task2
  ( doubleNeg
  , excludedNeg
  , pierce
  , doubleNegElim
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

{-|
  This function represents law of double negation adding.

  Преобразуем тип a -> Neg (Neg a).
  a -> (a -> Void) -> Void
  Частный случай 10 аксиомы ИИВ:
  a -> !a -> b = a -> (a -> Void) -> b
-}
doubleNeg :: a -> Neg (Neg a)
doubleNeg x notX = notX x

{-|
  This function represents law of excluded negation.

  (a|!a) доказуемо в КИВ, а значит
  !!(a|!a) доказуемо в ИИВ (по теореме Гливенко).
  Заселению типа соответствует доказательству
  формулы в ИИВ. Используем доказательство, знакомое нам по
  курсу математической логики.
-}
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = axiomNine (contraposition axiomSix) (contraposition axiomSeven)

axiomSix :: a -> Either a b
axiomSix a = Left a

axiomSeven :: b -> Either a b
axiomSeven b = Right b

-- (a -> b) -> (a -> (b -> Void)) -> (a -> Void)
axiomNine :: (a -> b) -> (a -> Neg b) -> Neg a
axiomNine f nb a  = nb a (f a)

-- (a -> b) -> ((b -> Void) -> (a -> Void))
contraposition :: (a -> b) -> (Neg b -> Neg a)
contraposition f nb a = nb (f a)

{-|
  This function represents pierce law.

  Изоморфизм Карри-Ховарда: формула доказуема в ИИВ <=> соответсвующий
  этой формуле тип населен.
  Из курса математической логики мы знаем, что закон
  Пирса ((a->b)->a)->a не доказуем в ИИВ.
  Чтобы показать это, воспользуемся корректоностью ИИВ.
  (Если формула доказуема в ИИВ, то она общезначима).
  Возьмем алгебру Гейтинга вида
  X = {1, 1/2, 0}, 1 <= 1/2 <= 0. -- Импликативная решетка с нулем.
  Напомним, что оценка в алгебрах Гейтинга задается следующим образом:
  [|a --> b|] = [|a|] -> [|b|], // Определение: c = a->b, c = max {x|x * a <= b}
  Пусть [|a|] = 0, [|b|] = 1/2, тогда:
  [|((a -> b) -> a) -> a|] = [|((a -> b) -> a)|] ->
  [|a|] = ([|(a -> b)|]  -> [|a|]) -> [|a|] =
  (([|a|] -> [|b|])  -> [|a|]) -> [|a|]
  ((1/2 -> 0) -> 1/2) -> 1/2 =
  (0 -> 1/2) -> 1/2 = 1 -> 1/2 = 1/2
  То есть при оценке переменных [|a|] = 0 [|b|] = 1/2 оценка исходной формулы
  [|((a -> b) -> a) -> a|] != 1, то есть формула (закон пирса) не является
  общезначимой, а следовательно недоказуема в ИИВ.
  Так как формула недоказуема в ИИВ, такой тип не является обитаемым
  в следствие изоморфизма Карри-Ховарда.
-}
pierce :: ((a -> b) -> a) -> a
pierce = undefined

{-|
  This function represents 10th axiom of classical logic.

  Аналогично рассуждениям в предыдущем пункте: покажем, что формулу
  !!a -> a нельзя доказать в ИИВ. Также возьмем алгебру Гейтинга
  X = {1, 1/2, 0}, 1 <= 1/2 <= 0.
  Пусть [|a|] = 1/2.
  Тогда оценка исходной формулы следующая:
  [|!!a -> a|] = [|!!a|] ->
  [|a|] = ~[|!a|] -> [|a|] =
  (~ (~[|a|])) -> [|a|] = (~ (~ 1/2)) -> 1/2 =
  (~ 0) -> 1/2 = 1 -> 1/2 = 1/2
  Таким образом при оценке [|a|] = 1/2, оценка исходной формулы
  [|!!a -> a|] != 1, таким образом исходная формула не является общезначимой,
  а значит и недоказуема в ИИВ (по теореме о корректности).
  Так как формула недоказуема в ИИВ, такой тип не является обитаемым
  в следствие изоморфизма Карри-Ховарда.
-}
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

{-|
  This function represents formula of propositional logic !!!a -> !a.

  !!!a -> !a можно легко получить из
  a -> !!a с помощью контрапозиции.
-}
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = contraposition doubleNeg
