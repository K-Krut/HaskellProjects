{-
1. визначення;
2. частково визначенi функцiї;
3. карринг;
4. секцiї (section);
5. оператор застосування $;
6. оператор композицiї .;
7. λ-функцiї i функцiї вищого порядку;
8. функцiї вищого порядку модуля Prelude.
-}



{-
1. Functions that take other functions as arguments, or return other functions, are called higher-order functions

-----------------------------

2. Частично определённые функции – это такие функции, которые определены не для всех значений аргументов.
Примером такой функции может быть функция поиска предыдущего числа для натуральных чисел.
Поскольку числа натуральные, то для нуля такого числа нет.
Для описания этого поведения мы можем воспользоваться специальным типом Maybe. Посмотрим на его определение:

Частично определённая функция имеет тип a -> Maybe b, если всё в порядке и значение было вычислено, она вернёт (Just a),
а в случае ошибки будет возвращено значение Nothing. Теперь мы можем определить нашу функцию так:

----------------------------

3.  func :: Int -> Double -> Char -> Bool => func :: Int -> (Double -> (Char -> Bool))  haskell принимает так

Т.е. функция func принимает параметр типа Int и возвращает новую функцию,
 которая принимает очередной параметр — типа Double и возвращает другую новую функцию,
  принимающую параметр типа Char и возвращающую значение типа Bool.
Преобразование функции от многих аргументов в функцию, берущую свои аргументы по одному называется каррированием.
Haskell автоматически выполняет каррирование всех функций, принимающих более одного параметра.
Именно благодаря каррированию становится возможным частичное применение функций, а так же создание сечений.
В свою очередь, частичное применение делает возможным существование бесточечной нотации.


curry :: ((a, b) -> c) -> a -> b -> c
uncurry :: (a -> b -> c) -> (a, b) -> c

Функция curry преобразовывает некаррированную функцию в каррированную.
Функция uncurry преобразовывает каррированную функцию в некаррированную.
Пример

msg :: Int -> Bool -> String
msg n True = show $ n `div` 2
msg n _ = show $ n * 2






------------------------------

4.(2^) (left section) is equivalent to (^) 2, or more verbosely \x -> 2 ^ x
  (^2) (right section) is equivalent to flip (^) 2, or more verbosely \x -> x ^ 2

Like partial application and lambda abstraction, sectioning provides a convenient way of writing some functions without having to explicitly name them:

(1+) (unsugared: (+) 1) is the "increment" function,
(2*) is the "double" function,


------------------------------
5. ($) :: (a -> b) -> a -> b
   f $ x = f x

It essentially changes function application to be right-associative instead of left-associative.
The main use of $ is to simplify some expressions by removing brackets. For example:

sum (map (+1) [1,2,3])  =>  sum $ map (+1) [1,2,3]

                                                                 приоритет
The thing to remember with $ is that it has the lowest possible precedence of all infix operators,
and so when you see it in an expression it is evaluated last


------------------------------
6.  f n = n + 1  |   g n = 2*n - 1
    h = f . g  -- h is the composition of f and g
    h(x) = f(g(x))

:type (.)        -- . must be written (.) to avoid a parser error
(.) :: (b -> c) -> (a -> b) -> a -> c
 h x is the same as f (g x).


Оператор композиции получает на вход две функции, а потом всего лишь даёт нам ЛФ, внутри которой происходит обыкновенный последовательный вызов этих двух функций через скобки. И никакой магии:

(.)    f        g        =  \x -> f (g x)

берём  эту      и эту       и возвращаем
       функцию  функцию     ЛФ, внутри
                            которой
                            вызываем их


------------------------------
7. \x -> x * x     |    (\x y -> x * y) 10 4
И если у обычной функции сначала идёт объявление/определение, а затем (где-то) применение с использованием имени,
то у ЛФ всё куда проще: мы её определяем и тут же применяем, на месте. Вот так:
(\x -> x * x) 5
ЛФ имеет тип, как и обычные данные. Но поскольку ЛФ является частным случаем функции — значит и у обыкновенной функции тоже есть тип!
------------------------------
8. map, filter, takeWhile, dropWhile, all, any, iterate
-}