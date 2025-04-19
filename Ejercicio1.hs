-- Tipos para términos (variables, constantes o funciones)
data Term = Var String          
          | Const String        
          | Func String [Term]  
          deriving (Show, Eq)

-- Tipos para fórmulas atómicas (predicados)
data Formula = Pred String [Term]                   
             | Not Formula                          -- Negación 
             | And Formula Formula                 -- Conjunción 
             | Or Formula Formula                  -- Disyunción 
             | Implies Formula Formula              -- Implicación 
             | Forall String Formula               -- Cuantificador universal 
             | Exists String Formula               -- Cuantificador existencial 
             deriving (Show, Eq)