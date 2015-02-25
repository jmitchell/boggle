module Boggle where

import Data.List
import Data.Maybe
import System.Random

type Letter = Char
type Dimensions = (Int, Int)
type Coordinate = (Int, Int)
type Tray = (Dimensions, Coordinate -> Maybe Letter)
type WordPath = [Coordinate]
type Vocabulary = [[Letter]]

-- | Create an empty tray with the specified dimensions.
--
-- Dimensions should be positive, but this constraint isn't checked.
emptyTray :: Dimensions -> Tray
emptyTray dims = (dims, const Nothing)

-- | List of tray coordinates with no assigned letters.
--
-- >>> emptyPositions $ emptyTray (1,2)
-- [(0,0),(0,1)]
emptyPositions :: Tray -> [Coordinate]
emptyPositions ((w,h), at) =
  [(x,y) | y <- [0..(h-1)], x <- [0..(w-1)], isNothing $ at (x,y)]

-- | Check if every valid coordinate on the tray has been assigned a
-- letter.
--
-- >>> isFull $ emptyTray (0,0)
-- True
--
-- >>> isFull $ emptyTray (1,1)
-- False
--
-- >>> isFull $ safeInsertLetter (emptyTray (1,1)) (0,0) 'x'
-- True
isFull :: Tray -> Bool
isFull = null . emptyPositions

-- | Check whether coordinate is within the bounds of the specified
-- dimensions.
--
-- >>> validCoordinate (3,5) (3,5)
-- False
--
-- >>> validCoordinate (3,5) (2,4)
-- True
--
-- >>> validCoordinate (3,5) (0,0)
-- True
--
-- >>> validCoordinate (3,5) (-1,2)
-- False
validCoordinate :: Dimensions -> Coordinate -> Bool
validCoordinate (w,h) (x,y) =
  x >= 0 && x < w &&
  y >= 0 && y < h

-- | Insert letter into tray at the specified coordinates. If the
-- coordinates are outside the tray's dimensions or another letter
-- already occupies that position, return the original tray.
safeInsertLetter :: Tray -> Coordinate -> Letter -> Tray
safeInsertLetter tray@((w,h), at) (x,y) letter =
  if validCoordinate (w,h) (x,y) && isNothing (at (x,y))
  then ((w,h), \coord -> if coord == (x,y)
                         then Just letter
                         else at coord)
  else tray

-- | Sequentially load letters into the tray until it's either full or
-- there are no more letters.
--
-- The process for selecting the next position to fill is
-- deterministic. Shuffle the list of letters beforehand if the goal
-- is to "shake" the tray.
--
-- >>> trayString '_' $ loadTray (emptyTray (2,3)) ['a'..]
-- "ab\ncd\nef"
loadTray :: Tray -> [Letter] -> Tray
loadTray tray letters =
  case (emptyPositions tray, letters) of
    ([], _)     -> tray
    (_, [])     -> tray
    (p:_, c:cs) -> loadTray newTray cs
      where
        newTray = safeInsertLetter tray p c

alphabet :: [Letter]
alphabet = ['a'..'z']

-- | Randomly select an element from the list.
--
-- Returns Nothing if the list is empty:
--
-- >>> pickRandomly []
-- Nothing
pickRandomly :: [a] -> IO (Maybe a)
pickRandomly elts =
  case elts of
    [] -> return Nothing
    _  -> do
      index <- randomRIO (0, (length elts)-1)
      return $ Just $ elts !! index

pickManyRandomly :: Int -> [a] -> IO [a]
pickManyRandomly n xs
  | n <= 0 || null xs = return []
  | otherwise         = do
    p <- pickRandomly xs
    ps <- pickManyRandomly (n-1) xs
    case p of
      Just p' -> do return (p':ps)
      _       -> do return ps

-- |
shuffledTray :: Dimensions -> IO Tray
shuffledTray (w,h) = do
  randomLetters <- pickManyRandomly (w*h) alphabet
  return $ loadTray (emptyTray (w,h)) randomLetters

-- | All valid coordinates neighboring the provided coordinate. There
-- can be up to 8 neighbors (4 sides and 4 diagonals).
--
-- >>> neighbors (2,2) (0,0)
-- [(1,0),(0,1),(1,1)]
--
-- >>> neighbors (3,3) (1,1)
-- [(0,0),(1,0),(2,0),(0,1),(2,1),(0,2),(1,2),(2,2)]
--
-- >>> neighbors (1,1) (0,0)
-- []
--
-- There are no more than 8 neighbors:
-- prop> length (neighbors d c) <= 8
--
-- No coordinate neighbors itself:
-- prop> not $ c `elem` (neighbors d c)
--
-- No neighbors fall outside the space defined by the dimensions parameter:
-- prop> not $ any (\(x,y) -> x < 0 || y < 0 || x >= w || y >= h) (neighbors (w,h) c)
--
-- Returned list of neighbors has no duplicates:
-- prop> neighbors d c == nub (neighbors d c)
neighbors :: Dimensions -> Coordinate -> [Coordinate]
neighbors (w,h) (x,y) =
  filter (validCoordinate (w,h)) [(x+i,y+j) | j <- [-1..1], i <- [-1..1], (i,j) /= (0,0)]

-- | Locate a letter on the tray. Returns a list of coordinates for
-- all matches.
--
-- >>> findLetter (emptyTray (1,1)) 'x'
-- []
--
-- >>> findLetter (loadTray (emptyTray (2,2)) "wxyz") 'x'
-- [(1,0)]
--
-- >>> findLetter (loadTray (emptyTray (2,2)) "xxxx") 'x'
-- [(0,0),(0,1),(1,0),(1,1)]
findLetter :: Tray -> Letter -> [Coordinate]
findLetter ((w,h), at) letter =
  [(x,y) | x <- [0..(w-1)], y <- [0..(h-1)], at (x,y) == Just letter]

-- | Locate neighbors matching the provided letter.
--
-- >>> findNeighboringLetter (loadTray (emptyTray (2,2)) "wxyz") (0,0) 'z'
-- [(1,1)]
findNeighboringLetter :: Tray -> Coordinate -> Letter -> [Coordinate]
findNeighboringLetter tray@(dims,_) fromCoord letter =
  filter (`elem` neighbors dims fromCoord) letterCoords
  where
    letterCoords = findLetter tray letter

-- | Find all paths on the Boggle tray which spell out the specified
-- word. No coordinate may be used multiple times in the same path.
--
-- >>> findWord (loadTray (emptyTray (1,1)) "x") "x"
-- [[(0,0)]]
--
-- >>> findWord (loadTray (emptyTray (1,1)) "A") "x"
-- []
--
-- >>> findWord (loadTray (emptyTray (2,2)) "x--x") "x"
-- [[(0,0)],[(1,1)]]
--
-- >>> findWord (loadTray (emptyTray (2,2)) "cat-") "cat"
-- [[(0,0),(1,0),(0,1)]]
--
-- >>> findWord (loadTray (emptyTray (2,2)) "patp") "pat"
-- [[(0,0),(1,0),(0,1)],[(1,1),(1,0),(0,1)]]
--
-- There are no words on an empty tray:
-- prop> findWord (emptyTray (w,h)) word == []
findWord :: Tray -> [Letter] -> [WordPath]
findWord tray word =
  case word of
    [] -> []
    (w:ws) -> map reverse $ findWord' ws $ initialPaths w
  where
    initialPaths letter = map (: []) $ findLetter tray letter
    findWordFromPath word path =
      case (word, path) of
        ("", _) -> [path]
        (w:ws, c:cs) -> findWord' ws [m:(c:cs) | m <- findNeighboringLetter tray c w, m `notElem` (c:cs)]
    findWord' w =
      concatMap (findWordFromPath w)

-- | Search tray for the provided words and return all that are found.
--
-- There are no words on an empty tray:
-- prop> wordsOnTray (emptyTray (w,h)) ws == []
wordsOnTray :: Tray -> [[Letter]] -> [[Letter]]
wordsOnTray tray = filter (not . null . findWord tray)

-- | String representation of a Tray.
--
-- >>> putStr $ trayString '*' $ emptyTray (3, 2)
-- ***
-- ***
--
-- >>> putStr $ trayString '_' $ emptyTray (4, 4)
-- ____
-- ____
-- ____
-- ____
--
-- >>> putStr $ trayString '_' (safeInsertLetter (safeInsertLetter (emptyTray (4, 4)) (2,0) 'x') (1,1) 'y')
-- __x_
-- _y__
-- ____
-- ____
trayString :: Char -> Tray -> String
trayString emptyChar ((w,h), at) =
  intercalate "\n" rows
  where
    displayLetter = fromMaybe emptyChar
    row y = [displayLetter $ at (x,y) | x <- [0..(w-1)]]
    rows = [row y | y <- [0..(h-1)]]

-- | Word list copied from http://www.becomeawordgameexpert.com/wordlists.htm#3letter
threeLetterWords :: Vocabulary
threeLetterWords =
  [ "aah", "aal", "aas", "aba", "abo", "abs", "aby", "ace", "act", "add", "ado", "ads", "adz", "aff", "aft", "aga", "age", "ago", "ags", "aha", "ahi", "ahs", "aid", "ail", "aim", "ain", "air", "ais", "ait", "ala", "alb", "ale", "all", "alp", "als", "alt", "ama", "ami", "amp", "amu", "ana", "and", "ane", "ani", "ant", "any", "ape", "apo", "app", "apt", "arb", "arc", "are", "arf", "ark", "arm", "ars", "art", "ash", "ask", "asp", "ass", "ate", "att", "auk", "ava", "ave", "avo", "awa", "awe", "awl", "awn", "axe", "aye", "ays", "azo", "baa", "bad", "bag", "bah", "bal", "bam", "ban", "bap", "bar", "bas", "bat", "bay", "bed", "bee", "beg", "bel", "ben", "bes", "bet", "bey", "bib", "bid", "big", "bin", "bio", "bis", "bit", "biz", "boa", "bob", "bod", "bog", "boo", "bop", "bos", "bot", "bow", "box", "boy", "bra", "bro", "brr", "bub", "bud", "bug", "bum", "bun", "bur", "bus", "but", "buy", "bye", "bys", "cab", "cad", "caf", "cam", "can", "cap", "car", "cat", "caw", "cay", "cee", "cel", "cep", "chi", "cig", "cis", "cob", "cod", "cog", "col", "con", "coo", "cop", "cor", "cos", "cot", "cow", "cox", "coy", "coz", "cru", "cry", "cub", "cud", "cue", "cum", "cup", "cur", "cut", "cuz", "cwm", "dab", "dad", "dag", "dah", "dak", "dal", "dam", "dan", "dap", "das", "daw", "day", "deb", "dee", "def", "del", "den", "dev", "dew", "dex", "dey", "dib", "did", "die", "dif", "dig", "dim", "din", "dip", "dis", "dit", "doc", "doe", "dog", "doh", "dol", "dom", "don", "dor", "dos", "dot", "dow", "dry", "dub", "dud", "due", "dug", "duh", "dui", "dun", "duo", "dup", "dye", "ear", "eat", "eau", "ebb", "eco", "ecu", "edh", "eds", "eek", "eel", "eff", "efs", "eft", "egg", "ego", "eke", "eld", "elf", "elk", "ell", "elm", "els", "eme", "emo", "ems", "emu", "end", "eng", "ens", "eon", "era", "ere", "erg", "ern", "err", "ers", "ess", "est", "eta", "eth", "eve", "ewe", "eye", "fab", "fad", "fag", "fah", "fan", "far", "fas", "fat", "fax", "fay", "fed", "fee", "feh", "fem", "fen", "fer", "fes", "fet", "feu", "few", "fey", "fez", "fib", "fid", "fie", "fig", "fil", "fin", "fir", "fit", "fix", "fiz", "flu", "fly", "fob", "foe", "fog", "foh", "fon", "foo", "fop", "for", "fou", "fox", "foy", "fro", "fry", "fub", "fud", "fug", "fun", "fur", "gab", "gad", "gae", "gag", "gal", "gam", "gan", "gap", "gar", "gas", "gat", "gay", "ged", "gee", "gel", "gem", "gen", "get", "gey", "ghi", "gib", "gid", "gie", "gif", "gig", "gin", "gip", "gis", "git", "gnu", "goa", "gob", "god", "goo", "gor", "gos", "got", "gox", "goy", "gul", "gum", "gun", "gut", "guv", "guy", "gym", "gyp", "had", "hae", "hag", "hah", "haj", "ham", "hao", "hap", "has", "hat", "haw", "hay", "heh", "hem", "hen", "hep", "her", "hes", "het", "hew", "hex", "hey", "hic", "hid", "hie", "him", "hin", "hip", "his", "hit", "hmm", "hob", "hod", "hoe", "hog", "hom", "hon", "hoo", "hop", "hos", "hot", "how", "hoy", "hub", "hue", "hug", "huh", "hum", "hun", "hup", "hut", "hyp", "ice", "ich", "ick", "icy", "ids", "iff", "ifs", "igg", "ilk", "ill", "imp", "ink", "inn", "ins", "ion", "ire", "irk", "ism", "its", "ivy", "jab", "jag", "jam", "jar", "jaw", "jay", "jee", "jet", "jeu", "jew", "jib", "jig", "jin", "job", "joe", "jog", "jot", "jow", "joy", "jug", "jun", "jus", "jut", "kab", "kae", "kaf", "kas", "kat", "kay", "kea", "kef", "keg", "ken", "kep", "kex", "key", "khi", "kid", "kif", "kin", "kip", "kir", "kis", "kit", "koa", "kob", "koi", "kop", "kor", "kos", "kue", "kye", "lab", "lac", "lad", "lag", "lah", "lam", "lap", "lar", "las", "lat", "lav", "law", "lax", "lay", "lea", "led", "lee", "leg", "lei", "lek", "les", "let", "leu", "lev", "lex", "ley", "lez", "lib", "lid", "lie", "lin", "lip", "lis", "lit", "lob", "log", "loo", "lop", "lor", "lot", "low", "lox", "lud", "lug", "lum", "luv", "lux", "lye", "mac", "mad", "mae", "mag", "mam", "man", "map", "mar", "mas", "mat", "maw", "max", "may", "med", "meg", "meh", "mel", "mem", "men", "mes", "met", "mew", "mho", "mib", "mic", "mid", "mig", "mil", "mim", "mir", "mis", "mix", "moa", "mob", "moc", "mod", "mog", "moi", "mol", "mom", "mon", "moo", "mop", "mor", "mos", "mot", "mow", "mud", "mug", "mum", "mun", "mus", "mut", "myc", "nab", "nae", "nag", "nah", "nam", "nan", "nap", "naw", "nay", "neb", "nee", "neg", "net", "new", "nib", "nil", "nim", "nip", "nit", "nix", "nob", "nod", "nog", "noh", "nom", "noo", "nor", "nos", "not", "now", "nth", "nub", "nun", "nus", "nut", "oaf", "oak", "oar", "oat", "oba", "obe", "obi", "oca", "och", "oda", "odd", "ode", "ods", "oes", "off", "oft", "ohm", "oho", "ohs", "oik", "oil", "oka", "oke", "old", "ole", "oma", "oms", "one", "ono", "ons", "oof", "ooh", "oot", "ope", "ops", "opt", "ora", "orb", "orc", "ore", "ors", "ort", "ose", "oud", "our", "out", "ova", "owe", "owl", "own", "owt", "oxo", "oxy", "pac", "pad", "pah", "pal", "pam", "pan", "pap", "par", "pas", "pat", "paw", "pax", "pay", "pea", "pec", "ped", "pee", "peg", "peh", "pen", "pep", "per", "pes", "pet", "pew", "phi", "pho", "pht", "pia", "pic", "pie", "pig", "pin", "pip", "pis", "pit", "piu", "pix", "ply", "pod", "poh", "poi", "pol", "pom", "poo", "pop", "pos", "pot", "pow", "pox", "pro", "pry", "psi", "pst", "pub", "pud", "pug", "pul", "pun", "pup", "pur", "pus", "put", "pya", "pye", "pyx", "qat", "qis", "qua", "rad", "rag", "rah", "rai", "raj", "ram", "ran", "rap", "ras", "rat", "raw", "rax", "ray", "reb", "rec", "red", "ree", "ref", "reg", "rei", "rem", "rep", "res", "ret", "rev", "rex", "rez", "rho", "ria", "rib", "rid", "rif", "rig", "rim", "rin", "rip", "rob", "roc", "rod", "roe", "rom", "roo", "rot", "row", "rub", "rue", "rug", "rum", "run", "rut", "rya", "rye", "sab", "sac", "sad", "sae", "sag", "sal", "san", "sap", "sat", "sau", "saw", "sax", "say", "sea", "sec", "see", "seg", "sei", "sel", "sen", "ser", "set", "sew", "sex", "sez", "sha", "she", "shh", "shy", "sib", "sic", "sim", "sin", "sip", "sir", "sis", "sit", "six", "ska", "ski", "sky", "sly", "sob", "soc", "sod", "soh", "sol", "som", "son", "sop", "sos", "sot", "sou", "sow", "sox", "soy", "spa", "spy", "sri", "sty", "sub", "sue", "suk", "sum", "sun", "sup", "suq", "sus", "syn", "tab", "tad", "tae", "tag", "taj", "tam", "tan", "tao", "tap", "tar", "tas", "tat", "tau", "tav", "taw", "tax", "tea", "tec", "ted", "tee", "teg", "tel", "ten", "tes", "tet", "tew", "the", "tho", "thy", "tic", "tie", "til", "tin", "tip", "tis", "tit", "tix", "tod", "toe", "tog", "tom", "ton", "too", "top", "tor", "tot", "tow", "toy", "try", "tsk", "tub", "tug", "tui", "tum", "tun", "tup", "tut", "tux", "twa", "two", "tye", "udo", "ugh", "uke", "ulu", "umm", "ump", "uni", "uns", "upo", "ups", "urb", "urd", "urn", "urp", "use", "uta", "ute", "uts", "vac", "van", "var", "vas", "vat", "vau", "vav", "vaw", "vee", "veg", "vet", "vex", "via", "vid", "vie", "vig", "vim", "vin", "vis", "voe", "vow", "vox", "vug", "vum", "wab", "wad", "wae", "wag", "wan", "wap", "war", "was", "wat", "waw", "wax", "way", "web", "wed", "wee", "wen", "wet", "wha", "who", "why", "wig", "win", "wis", "wit", "wiz", "woe", "wog", "wok", "won", "woo", "wop", "wos", "wot", "wow", "wry", "wud", "wye", "wyn", "xis", "yag", "yah", "yak", "yam", "yap", "yar", "yaw", "yay", "yea", "yeh", "yen", "yep", "yes", "yet", "yew", "yid", "yin", "yip", "yob", "yod", "yok", "yom", "yon", "you", "yow", "yuk", "yum", "yup", "zag", "zap", "zas", "zax", "zed", "zee", "zek", "zep", "zig", "zin", "zip", "zit", "zoa", "zoo", "zuz" ]
