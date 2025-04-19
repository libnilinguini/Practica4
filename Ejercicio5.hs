interpAlicia :: Interpretacion
interpAlicia = (
    \x -> Just (Const x), -- Asignación trivial de variables
    \predName args -> case (predName, args) of
        ("esAdministrador", [Const "Alicia"]) -> True
        ("esUsuario", [Const "Alicia"])       -> True
        ("tieneLlave", [Const "Alicia"])      -> True  -- podría derivarse por regla2
        ("accesoPermitido", [Const "Alicia"]) -> True  -- lo que queremos verificar
        _ -> False
  )
