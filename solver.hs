import Data.List
import System.IO

-- Znaków używane do reprezentacji różnych pól
empty = '+' -- pola o nieznanej wartości
house = 'H' -- pola z domami
cont = 'O'  -- pola ze zbiornikami
mark = ' '  -- pola oznaczone jako puste

-- Przechowuje dane dotyczące przetwarzanej planszy
data Board = Board { board :: [[Char]]     -- wartości na poszczególnych polach planszy
                   , x_n   :: [Int]        -- ilości zbiorników w poszczególnych wierszach
                   , y_n   :: [Int]        -- ilości zbiorników w poszczególnych kolumnach
                   , x_s   :: Int          -- ilość kolumn (poziomy rozmiar planszy)
                   , y_s   :: Int          -- ilość wierszy (pionowy rozmiar planszy)
                   , xys   :: [(Int, Int)] -- wszystkie możliwe pozycje na planszy (pary (x, y))
                   , ns    :: [Int]        -- wszystkie możliwe pozycje na planszy (liniowe indeksy)
                   } deriving (Show)

-- Tworzy zainicjowaną planszę na podstawie danych wejściowych zadania
make_board :: [Int] -> [Int] -> [(Int, Int)] -> Board
make_board rows_n cols_n houses = mark_away (place_houses new_board houses)
                                  where new_board = Board { board = blank_board
                                                          , x_n = rows_n
                                                          , y_n = cols_n
                                                          , x_s = x_size
                                                          , y_s = y_size
                                                          , xys = map (\n -> ((mod n x_size), (div n x_size))) [0 .. n_max]
                                                          , ns = [0 .. n_max]
                                                          }
                                        blank_board = [[empty | i <- [1 .. x_size]] | j <- [1 .. y_size]]
                                        x_size = length cols_n
                                        y_size = length rows_n
                                        n_max = x_size * y_size - 1

-- Umieszcza na planszy domy na wszystkich podanych w liście pozycjach
place_houses :: Board -> [(Int, Int)] -> Board
place_houses b [] = b
place_houses b ((y, x):yxs) = place_houses (update_board b (x, y) house) yxs

-- Aktualizuje na planszy wartość pola o wskazanych współrzędnych
update_board :: Board -> (Int, Int) -> Char -> Board
update_board b (x, y) v = b { board = take y (board b) ++
                                      [take x ((board b) !! y) ++ [v] ++ drop (x + 1) ((board b) !! y)] ++
                                      drop (y + 1) (board b) }

-- Umieszcza zbiornik na polu planszy o podanych współrzędnych i oznacza pola dookoła jako puste
place_cont :: Board -> (Int, Int) -> Board
place_cont board (x, y) = mark_if_empty_xy (
                            mark_if_empty_xy (
                              mark_if_empty_xy (
                                mark_if_empty_xy (
                                  mark_if_empty_xy (
                                    mark_if_empty_xy (
                                      mark_if_empty_xy (
                                        mark_if_empty_xy (
                                          update_board board (x, y) cont
                                        ) (x-1, y-1)
                                      ) (x, y-1)
                                    ) (x+1, y-1)
                                  ) (x-1, y)
                                ) (x+1, y)
                              ) (x-1, y+1)
                            ) (x, y+1)
                          ) (x+1, y+1)

-- Zwraca wartość pola o podanych współrzędnych. Dla błędnych współrzędnych zwraca '\0'
at :: Board -> (Int, Int) -> Char
at b (x, y) | x < 0         = '\0'

            | x >= (x_s b)  = '\0'
            | y < 0         = '\0'
            | y >= (y_s b)  = '\0'
            | otherwise = (board b) !! y !! x

-- Oznacza pole na planszy jako puste, jeśli nic się tam nie znajduje
mark_if_empty_xy :: Board -> (Int, Int) -> Board
mark_if_empty_xy b (x, y) | empty_at b (x, y) = update_board b (x, y) mark
                          | otherwise         = b
-- Wstawia na polu na planszy zbiornik, o ile nic się w nim nie znajduje
cont_if_empty_xy :: Board -> (Int, Int) -> Board
cont_if_empty_xy b (x, y) | empty_at b (x, y) = place_cont b (x, y)
                          | otherwise         = b

-- Wyświetla planszę wraz z poprawnymi liczbami zbiorników w wierszach i kolumnach
display :: Board -> IO ()
display b = do
               putStrLn ("  " ++ (map (!! 0) (map show (y_n b)))) -- wyświetl liczbę zbiorników w kolumnach
               putStr (display' (x_n b) (y_s b) (board b))
            where 
                  -- wyświetla wiersze planszy poprzedzone liczbą zbiorników, jakie powinny się w nich znajdować
                  display' x_n y_s [] = ""
                  display' x_n y_s (row:rows) = (show (x_n !! (y_s - (length rows) - 1)))
                                                    ++ " " ++ row ++ "\n"
                                                    ++ (display' x_n y_s rows)

-- Zwraca współrzędne pola o podanym indeksie linowym
n2xy :: Board -> Int -> (Int, Int)
n2xy b n = ((mod n (x_s b)), (div n (x_s b)))
-- Zwraca indeks linowy pola o podanych współrzędnych
xy2n :: Board -> (Int, Int) -> Int
xy2n b (x, y) = y * (x_s b) + x

-- Zwraca indeksy liniowe pól, które znajdują się w wierszu planszy o danej współrzędnej y
row_ns :: Board -> Int -> [Int]
row_ns b y = [i + y * (x_s b) | i <- [0 .. ((x_s b) - 1)]]
-- Zwraca indeksy liniowe pól, które znajdują się w kolumnie planszy o danej współrzędnej x
col_ns :: Board -> Int -> [Int]
col_ns b x = [i * (x_s b) + x | i <- [0 .. ((y_s b) - 1)]]

-- Sprawdza, czy na podanym polu planszy znajduje się dom
house_at :: Board -> (Int, Int) -> Bool
house_at b (x, y) = at b (x, y) == house
-- Sprawdza, czy na podanym polu planszy znajduje się zbiornik
cont_at :: Board -> (Int, Int) -> Bool
cont_at b (x, y) = at b (x, y) == cont
-- Sprawdza, czy na podanym polu planszy nic się nie znajduje
empty_at :: Board -> (Int, Int) -> Bool
empty_at b (x, y) = at b (x, y) == empty

-- Oznacza jako puste pola na planszy, które nie sąsiadują z żadnym domem
mark_away :: Board -> Board
mark_away b = mark_away' b (ns b)
mark_away' b [] = b
mark_away' b (n:ns) | is_away b (n2xy b n) = mark_away' (update_board b (n2xy b n) mark) ns
                    | otherwise = mark_away' b ns
                    where is_away b (x, y) = length (find_at b ((x, y):(xy_plus (x, y))) house) == 0

-- Zwraca ilość zbiorników umieszczonych aktualnie na planszy
count_cont :: Num a => Board -> [Int] -> a
count_cont b [] = 0
count_cont b (n:ns) | cont_at b (n2xy b n) = 1 + count_cont b ns
                    | otherwise = count_cont b ns

-- Zwraca ilość zbiorników umieszczonych na planszy i miejsc, na których nic się nie znajduje
count_empty_cont :: Num a => Board -> [Int] -> a
count_empty_cont b [] = 0
count_empty_cont b (n:ns) | empty_at b (n2xy b n) || cont_at b (n2xy b n) = 1 + count_empty_cont b ns
                          | otherwise = count_empty_cont b ns

-- Oznacza podane pola planszy jako puste, jeśli nic się na nich nie znajduje
mark_if_empty :: Board -> [Int] -> Board
mark_if_empty b [] = b
mark_if_empty b (n:ns) | empty_at b (n2xy b n) = mark_if_empty (update_board b (n2xy b n) mark) ns
                       | otherwise = mark_if_empty b ns
-- Umieszcza na podanych polach zbiorniki, jeśli nic się na nich nie znajduje
cont_if_empty :: Board -> [Int] -> Board
cont_if_empty b [] = b
cont_if_empty b (n:ns) | empty_at b (n2xy b n) = cont_if_empty (place_cont b (n2xy b n)) ns
                       | otherwise = cont_if_empty b ns

-- Uzupełnia wiersze, w których brakująca ilość 
fill_known :: Board -> Board
fill_known b = fill_known'' b (fill_known' b)
fill_known' b = fill_known_cols (fill_known_rows b (x_n b)) (y_n b)
fill_known'' b1 b2 | (board b1) == (board b2) = b1
                   | otherwise = fill_known'' b2 (fill_known' b2)
fill_known_rows b [] = b
fill_known_rows b (y:ys) | y == 0 = fill_known_rows (mark_if_empty b (row_ns b ((y_s b) - (length ys) - 1))) ys
                         | y == count_empty_cont b (row_ns b ((y_s b) - (length ys) - 1)) = fill_known_rows (cont_if_empty b (row_ns b ((y_s b) - (length ys) - 1))) ys
                         | y == count_cont b (row_ns b ((y_s b) - (length ys) - 1)) = fill_known_rows (mark_if_empty b (row_ns b ((y_s b) - (length ys) - 1))) ys
                         | otherwise = fill_known_rows b ys
fill_known_cols b [] = b
fill_known_cols b (x:xs) | x == 0 = fill_known_cols (mark_if_empty b (col_ns b ((x_s b) - (length xs) - 1))) xs
                         | x == count_empty_cont b (col_ns b ((x_s b) - (length xs) - 1)) = fill_known_cols (cont_if_empty b (col_ns b ((x_s b) - (length xs) - 1))) xs
                         | x == count_cont b (col_ns b ((x_s b) - (length xs) - 1)) = fill_known_cols (mark_if_empty b (col_ns b ((x_s b) - (length xs) - 1))) xs
                         | otherwise = fill_known_cols b xs

-- Zlicza pola danego typu występujące wokół pola o danych współrzędnych
count_around :: Board -> (Int, Int) -> Char -> Int
count_around b (x, y) v = length (find_at b (xy_plus (x, y)) v)

-- Umieszcza zbiorniki obok domów, które nie mają zbiornika, a posiadają tylko jedno wolne pole
fill_around_houses :: Board -> Board
fill_around_houses b = fill_around_houses' b (ns b)
fill_around_houses' b [] = b
fill_around_houses' b (n:ns) | house_at b (n2xy b n) = fill_around_houses' (fill_around_houses'' b (n2xy b n)) ns
                             | otherwise             = fill_around_houses' b ns
fill_around_houses'' b (x, y) | (count_around b (x, y) empty == 1) 
                                  && (count_around b (x, y) cont == 0) = cont_if_empty_xy (
                                                                           cont_if_empty_xy (
                                                                             cont_if_empty_xy (
                                                                               cont_if_empty_xy b (x-1, y)
                                                                             ) (x+1, y)
                                                                           ) (x, y-1)
                                                                         ) (x, y+1)
                              | otherwise = b

-- Zwraca pozycje sasiadujace z podaną (gora, dol, prawo, lewo)
xy_plus :: (Num t, Num t1) => (t, t1) -> [(t, t1)]
xy_plus (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
-- Zwraca pozycje sasiadujace z podaną po skosie
xy_cross :: (Num t, Num t1) => (t, t1) -> [(t, t1)]
xy_cross (x, y) = [(x-1, y-1), (x+1, y-1), (x+1, y-1), (x+1, y+1)]
-- Zwraca pozycje sasiadujace z podaną (góra, dół, boki, skosy)
xy_all :: (Num t, Num t1) => (t, t1) -> [(t, t1)]
xy_all (x, y) = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
-- Zwraca pozycje, na których mogą znajdować się zablokowane domy, jeśli na podanej pozycji umieści się zbiornik
xy_blocked :: (Num t, Num a) => (a, t) -> [(a, t)]
xy_blocked (x, y) = (xy_all (x, y)) ++ (map (\xy -> (fst xy + x, snd xy + y)) [(-1,2),(0,2),(1,2),(2,1),(2,0),(2,-1),(-1,-2),(0,-2),(1,-2),(-2,1),(-2,0),(-2,-1)])

-- Dokonuje operacji na podanej planszy dla wszystkich podanych punktów
for_all_points :: t1 -> (t1 -> t -> t1) -> [t] -> t1
for_all_points b func [] = b
for_all_points b func (point:points) = for_all_points (func b point) func points

-- Zwraca te elementy z podanej listy, pod którymi znajdują się elementy v
find_at :: Board -> [(Int, Int)] -> Char -> [(Int, Int)]
find_at b points v = filter (\(x,y) -> at b (x, y) == v) points

-- Wypelnia znakiem mark puste pole jesli w pobliżu jest tylko jeden dom
mark_empty_near_house :: Board -> (Int, Int) -> Board
mark_empty_near_house b (x, y) | length houses == 1 = update_board b (x, y) mark
                                   | otherwise          = b
                                   where
                                       houses = find_at b (xy_plus (x, y)) house

-- Wywoluje funkcję mark_empty_around_house dla domów które są jedyne w pobliżu zbiorników
fill_around_conts :: Board -> Board
fill_around_conts b = fill_around_conts' b (ns b)
fill_around_conts' b [] = b
fill_around_conts' b (n:ns) | cont_at b xy && length houses == 1 = fill_around_conts' (fill_around_conts'' b (houses !! 0)) ns
                            | otherwise = fill_around_conts' b ns
                            where xy = n2xy b n
                                  houses = find_at b (xy_plus xy) house
fill_around_conts'' b (x, y) = for_all_points b mark_empty_near_house (find_at b (xy_plus (x, y)) empty)

-- Sprawdza, czy w ciągu znajdują się podciągi złożone z empty dłuższe niż [empty, empty]
fill_around_conts' :: Board -> [Int] -> Board
three_or_more_empty a = isInfixOf [empty,empty,empty] a

-- Zlicza ilość kontenerów w wierszu planszy
count_conts_row :: Board -> Int -> Int
count_conts_row b r = (length . filter (cont==)) ((board b) !! r)
count_conts_rows b r_max = map (count_conts_row b) [0 .. r_max]
-- Zlicza ilość kontenerów w kolumnie planszy
count_conts_col :: Board -> Int -> Int
count_conts_col b c = (length . filter (cont==)) (map (!! c) (board b))
count_conts_cols b c_max = map (count_conts_col b) [0 .. c_max]

-- Wypełnia jedno i dwu elementowe ciągi wolnych pól jeśli ich ilość odpowiada liczbie brakujących zbiorników
fill_empty_ones_twos :: Board -> Board
fill_empty_ones_twos b = fill_empty_in_cols (fill_empty_in_rows b [0 .. (y_s b)-1]) [0 .. (x_s b)-1]
fill_empty_in_rows b [] = b
fill_empty_in_rows b (r:rows) = fill_empty_in_rows (fill_empty_in_rows' b r) rows
fill_empty_in_rows' b r | (not (three_or_more_empty ((board b) !! r)))
                            && (((x_n b) !! r) - (count_conts_row b r) == count_empty_substrs ((board b) !! r)) = fill_empty_in_rows'' b (row_ns b r)
                        | otherwise = b
fill_empty_in_rows'' b [] = b
fill_empty_in_rows'' b (n:m:ns) | empty_at b nxy && empty_at b mxy = fill_empty_in_rows'' (fill_empty_in_rows_two b nxy) ns
                                | empty_at b nxy = fill_empty_in_rows'' (fill_empty_in_rows_one b nxy) (m:ns)
                                | otherwise = fill_empty_in_rows'' b (m:ns)
                                where nxy = n2xy b n
                                      mxy = n2xy b m
fill_empty_in_rows'' b (n:ns) | empty_at b nxy = fill_empty_in_rows'' (fill_empty_in_rows_one b nxy) ns
                              | otherwise = fill_empty_in_rows'' b ns
                              where nxy = n2xy b n
fill_empty_in_rows_two b (x, y) = mark_if_empty_xy (mark_if_empty_xy (mark_if_empty_xy (mark_if_empty_xy b (x, y-1)) (x, y+1)) (x+1, y-1)) (x+1, y+1)
fill_empty_in_rows_one b (x, y) = place_cont b (x, y)
fill_empty_in_cols b [] = b
fill_empty_in_cols b (c:cols) = fill_empty_in_cols (fill_empty_in_cols' b c) cols
fill_empty_in_cols' b c | (not (three_or_more_empty (map (!! c) (board b))))
                             && (((y_n b) !! c) - (count_conts_col b c) == count_empty_substrs (map (!! c) (board b))) = fill_empty_in_cols'' b (col_ns b c)
                        | otherwise = b
fill_empty_in_cols'' b [] = b
fill_empty_in_cols'' b (n:m:s) | empty_at b nxy && empty_at b mxy = fill_empty_in_cols'' (fill_empty_in_cols_two b nxy) s
                               | empty_at b nxy = fill_empty_in_cols'' (fill_empty_in_cols_one b nxy) (m:s)
                               | otherwise = fill_empty_in_cols'' b (m:s)
                               where nxy = n2xy b n
                                     mxy = n2xy b m
fill_empty_in_cols'' b (n:s) | empty_at b nxy = fill_empty_in_cols'' (fill_empty_in_cols_one b nxy) s
                             | otherwise = fill_empty_in_cols'' b s
                             where nxy = n2xy b n
fill_empty_in_cols_two b (x, y) = mark_if_empty_xy (mark_if_empty_xy (mark_if_empty_xy (mark_if_empty_xy b (x-1, y)) (x+1, y)) (x-1, y+1)) (x+1, y+1)
fill_empty_in_cols_one b (x, y) = place_cont b (x, y)

-- Zwraca ilość podciągów [empty] i [empty, empty] w ciągu
count_empty_substrs :: Num a => [Char] -> a
count_empty_substrs [] = 0
count_empty_substrs (a:b:s) | a == empty && b == empty = 1 + count_empty_substrs s
                            | a == empty               = 1 + count_empty_substrs (b:s)
                            | otherwise                = count_empty_substrs (b:s)
count_empty_substrs (a:s) | a == empty = 1 + count_empty_substrs s
                          | otherwise  = count_empty_substrs s


-- Przechodzi po odizolowanych ciągach zbiorników i domów, i zwraca wartość odpowiadającą sumie wag domów (-1) i zbiorników (+1)
-- jeśli cnt_empty == True, to funkcja zlicza także pola puste z wagą +1
transverse :: Num a => Board -> (Int, Int) -> [(Int, Int)] -> Bool -> a
transverse b xy lastxy cnt_empty | house_at b xy = (-1) + next_val
                                 | cont_at b xy  = 1 + next_val
                                 | empty_at b xy = if cnt_empty then 1 else 0
                                 | otherwise         = 0
                                 where next_val = sum (map (\x -> transverse b x (x:xy:lastxy) cnt_empty) (transverse_find b xy (xy:lastxy)))
                                       transverse_find b xy lastxy = (find_at b nextxy cont) ++ (find_at b nextxy house) ++ (find_at b nextxy empty)
                                       nextxy = filter (\x -> not (elem x lastxy)) (xy_plus xy)

-- Wypełnia na planszy puste pola, które rozpoczynają/kończą odizolowane ciągi zbiorników i domów
fill_transversed :: Board -> Board
fill_transversed b = fill_transversed' b (ns b)
fill_transversed' b [] = b
fill_transversed' b (n:ns) | empty_at b nxy && length hs > 0 && tsum1 = fill_transversed' (place_cont b nxy) ns
                           | empty_at b nxy && length hs == 1 && tsum2 == 0 && hs_cont > 0 = fill_transversed' (update_board b nxy mark) ns
                           | otherwise = fill_transversed' b ns
                           where nxy = n2xy b n
                                 hs = find_at b (xy_plus nxy) house
                                 tsum1 = or (map (\x -> transverse b x [nxy] True == (-1)) hs)
                                 tsum2 = transverse b (hs !! 0) [nxy] False
                                 hs_cont = length (find_at b (xy_plus (hs !! 0)) cont)

-- Zwraca listę punktów na których znajdują się pola empty sąsiadujące z podanym punktem
empty_around_house :: Board -> (Int, Int) -> [(Int, Int)]
empty_around_house b (x, y) = find_at b (xy_plus (x, y)) empty

-- Sprawdza, czy postawienie cont na danym polu empty powoduje zablokowanie innego domu
will_block_house :: Board -> (Int, Int) -> (Int, Int) -> Bool
will_block_house b (x, y) base_house_xy = or (map (\xy -> and (map (\x -> elem x blocked_xys) (houses_empty_xys xy))) possible_houses_xys)
                                            where possible_houses_xys = filter (\xy -> count_around b xy cont == 0  && xy /= base_house_xy) (find_at b (xy_blocked (x, y)) house)
                                                  houses_empty_xys (x, y) = find_at b (xy_plus (x, y)) empty
                                                  blocked_xys = (x, y):(xy_all (x, y))

-- Umieszcza zbiorniki przy domach, które posiadają tylko jedno miejsce, które nie powoduje zablokowania wszystkich wolnych pól innego domu
fill_colliding :: Board -> Board
fill_colliding b = fill_colliding' b (ns b)
fill_colliding' b [] = b
fill_colliding' b (n:ns) | house_at b nxy && no_cont 
                             && (length (collisions) - length (filter (True==) collisions)) == 1 = fill_colliding' (place_cont b (houses_empty_xys !! false_index)) ns
                         | otherwise = fill_colliding' b ns
                         where nxy = n2xy b n
                               no_cont = length (find_at b (xy_plus nxy) cont) == 0
                               houses_empty_xys = find_at b (xy_plus nxy) empty
                               collisions = map (\xy -> will_block_house b xy nxy) houses_empty_xys
                               false_index = (\(Just i) -> i) (elemIndex False collisions)

-- Zwraca indeksy pustych pól na planszy
find_empty_ns :: Board -> [Int]
find_empty_ns b = map (xy2n b) (find_at b (xys b) empty)

-- Zwraca współrzędne domów, które nie posiadają i nie będą posiadały w przyszłości swojego zbiornika (ze wsględu na błąd w rozwiązaniu)
find_abandoned_houses :: Board -> [(Int, Int)]
find_abandoned_houses b = filter (\xy -> ((find_at b (xy_plus xy) empty) ++ (find_at b (xy_plus xy) cont)) == []) all_houses
                            ++ filter (\xy -> (transverse b xy [xy] True) <= (-2)) all_conts -- zbiorniki nalezace do wiecej niz 1 domu
                          where all_houses = (find_at b (xys b) house)
                                all_conts = (find_at b (xys b) cont)

-- Wypełnia planszę na podstawie znanych zależności
solve :: Board -> Board
solve b = solve_step' b (solve_step'' b)
solve_step'' b = (fill_colliding . fill_transversed . fill_known . fill_around_houses . fill_around_conts . fill_empty_ones_twos) b
solve_step' b1 b2 | (board b1) == (board b2) = b1
                  | otherwise                = solve_step' b2 (solve_step'' b2)

-- Sprawdza, czy rozwiązanie planszy jest kompletne
board_complete :: Board -> Bool
board_complete b = (count_conts_rows b ((y_s b) - 1) == (x_n b)) && (count_conts_cols b ((x_s b) - 1) == (y_n b))

-- Sprawdza, czy w rozwiązaniu planszy występują nieprawidłowości
board_problem :: Board -> Bool
board_problem b = (count_conts_rows b ((y_s b) - 1) > (x_n b)) || (count_conts_cols b ((x_s b) - 1) > (y_n b)) || length (find_abandoned_houses b) > 0

-- Zwraca indeks kolejnego pustego pola (począwszy od n+1)
next_empty :: Board -> Int -> Int
next_empty b n | n >= ((x_s b) * (y_s b) - 1) = -2 -- zwróć -2 jeśli nie znaleziono żadnego pustego pola
               | empty_at b (n2xy b (n+1))    = n+1
               | otherwise                    = next_empty b (n+1)

-- Przeszukuje wgłąb przestrzeń rozwiązań wstawiając w wolne pola zbiorniki
find_solution :: Board -> Board
find_solution b = if sn /= (-2) then find_solution' sb (sn-1) else sb
                  where sb = solve b
                        sn = next_empty sb (-1)
find_solution' b n | board_problem bC  = result_mark
                   | board_complete bC = bC
                   | otherwise         = if (board bC') == board (error_board)
                                         then result_mark
                                         else bC'
                   where bC = solve (place_cont b next_xy)
                         bC' = if next_n_C then (find_solution' bC next_n) else error_board
                         bM = solve (update_board b next_xy mark)
                         bM' = if next_n_M then (find_solution' bM next_n) else error_board
                         next_n = next_empty b n
                         next_xy = n2xy b next_n
                         next_n_C = next_empty bC next_n /= (-2)
                         next_n_M = next_empty bM next_n /= (-2)
                         result_mark = if board_problem bM
                                       then error_board
                                       else if board_complete bM
                                            then bM
                                            else bM'
                         error_board = b { board = [[]] }

-- Zapisuje rozwiązanie do pliku
write :: FilePath -> Board -> IO ()
write f_name b = do
                    writeFile f_name (write' (board b))
                    putStrLn ("Zapisano rozwiązanie do pliku " ++ show f_name ++ ".")
                 where write' [] = ""
                       write' (row:rows) = row ++ "\n" ++ (write' rows)

-- Funkcja główna
main :: IO ()
main = do
          -- Pobranie nazwy pliku wejściowego od użytkownika
          putStr "Podaj nazwę pliku wejściowego: "
          hFlush stdout
          f_name <- getLine

          -- Wydobycie z pliku danych wejściowych
          f_content <- readFile f_name
          let ls = lines f_content
          let rows = read (ls !! 0) :: [Int]
          let cols = read (ls !! 1) :: [Int]
          let houses = read (ls !! 2) :: [(Int, Int)]

          -- Utworzenie planszy na podstawie danych wejściowych i jej rozwiązanie
          let board0 = make_board rows cols houses
          let board1 = find_solution board0

          -- Wyświetlenie rozwiązania
          putStrLn "Rozwiązanie:"
          display board1
          
          -- Pobranie nazwy pliku wyjściowego i zapisanie roziwązania
          putStr "Podaj nazwę pliku wyjściowego: "
          hFlush stdout
          f_name <- getLine
          write f_name board1
