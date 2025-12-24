#set page(flipped: false)

#table(
  columns: (1fr, 1fr),
  inset: 10pt,
  stroke: none, // Remove borders for a cleaner "logic sheet" look
  align: center + horizon,
  
  [(T-Int)], 
  [$ ()/(Gamma tack.r n: "Int") $],

  [(T-Bool)], 
  [$ ()/(Gamma tack.r "true": "Bool") quad ()/(Gamma tack.r "false": "Bool") $],

  [(T-Var)], 
  [$ (x : T in Gamma)/(Gamma tack.r x : T) $],

  [(T-Lambda)], 
  [$ (Gamma, x : T_1 tack.r t_2 : T_2)/(Gamma tack.r lambda x . t_2 : T_1 -> T_2) $],

  [(T-App)], 
  [$ (Gamma tack.r t_1 : T_11 -> T_12 quad Gamma tack.r t_2 : T_11)/(Gamma tack.r t_1 space t_2 : T_12) $],

  [(T-Succ)], 
  [$ (Gamma tack.r t : "Int")/(Gamma tack.r "succ" space t : "Nat") $],

  [(T-Pred)], 
  [$ (Gamma tack.r t : "Int")/(Gamma tack.r "pred" space t : "Nat") $],

  [(T-IsZero)], 
  [$ (Gamma tack.r t : "Int")/(Gamma tack.r "iszero" space t : "Bool") $],

  [(T-If)], 
  [$ (Gamma tack.r t_1 : "Bool" quad Gamma tack.r t_2 : T quad Gamma tack.r t_3 : T)/(Gamma tack.r "if" t_1 "then" t_2 "else" t_3 : T) $],

  [(T-Let)], 
  [$ (Gamma tack.r t_1 : T_1 quad Gamma tack.r t_2 : T_2)/(Gamma tack.r "let" x =  t_1 "in" t_2 : T_2) $]
)
