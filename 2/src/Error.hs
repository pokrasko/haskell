module Error where


data Error = ParseError
           | ZeroDivision
           | MissingBinding String
           | MultipleBindings String

instance Show Error where
    show ParseError           = "Parsing error"
    show ZeroDivision         = "Division by 0"
    show (MissingBinding x)   = "No binding for " ++ x
    show (MultipleBindings x) = "Multiple bindings for " ++ x


boolEither :: Bool -> a -> b -> Either a b
boolEither False l _ = Left l
boolEither True  _ r = Right r
