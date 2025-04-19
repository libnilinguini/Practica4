type Interpretacion = (String -> Maybe Term, String -> [Term] -> Bool)

-- Función que evalúa un término bajo una interpretación
evalTerm :: Interpretacion -> Term -> Term
evalTerm (varInt, _) (Var x) = case varInt x of
                                Just t -> t
                                Nothing -> Var x
evalTerm _ t = t

-- Función que verifica si una fórmula es verdadera bajo una interpretación
satisface :: Interpretacion -> Formula -> Bool
satisface int (Pred p terms) = 
    let evalTerms = map (evalTerm int) terms
    in (snd int) p evalTerms
satisface int (Not f) = not (satisface int f)
satisface int (And f1 f2) = satisface int f1 && satisface int f2
satisface int (Or f1 f2) = satisface int f1 || satisface int f2
satisface int (Implies f1 f2) = not (satisface int f1) || satisface int f2
-- Para cuantificadores (simplificado, sin manejo de dominios infinitos)
satisface int@(varInt, predInt) (Forall x f) =
    -- Asumimos que podemos enumerar posibles valores (ej: Const "Alicia", Const "Bob")
    let posiblesValores = [Const "Alicia", Const "Bob"]  -- Dominio finito
    in all (\v -> satisface ((\y -> if y == x then Just v else varInt y), predInt) f) posiblesValores
satisface int@(varInt, predInt) (Exists x f) =
    let posiblesValores = [Const "Alicia", Const "Bob"]
    in any (\v -> satisface ((\y -> if y == x then Just v else varInt y), predInt) f) posiblesValores