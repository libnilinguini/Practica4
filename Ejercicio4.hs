-- Ejemplo de reglas:
regla1 :: Formula
regla1 = Implies (And (Pred "esUsuario" [Var "x"]) (Pred "tieneLlave" [Var "x"])) 
                 (Pred "accesoPermitido" [Var "x"])

regla2 :: Formula
regla2 = Implies (Pred "esAdministrador" [Var "x"]) (Pred "tieneLlave" [Var "x"])

-- Base de conocimiento (reglas + hechos)
baseConocimiento :: [Formula]
baseConocimiento = [regla1, regla2, Pred "esAdministrador" [Const "Alicia"]]

verificarAcceso :: String -> Bool
verificarAcceso nombre =
    let hecho = Pred "accesoPermitido" [Const nombre]
        interpretaciones = [interpAlicia]  -- podrías agregar más si quieres
    in consecuenciaLogica baseConocimiento hecho interpretaciones
