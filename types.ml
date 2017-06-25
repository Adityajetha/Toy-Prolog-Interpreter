type variable = Var of string;;
type symbol = Sym of string;;
type constant = Cons of string;;

type term = V of variable | C of constant | Func of symbol * (term list);;

type atomic_formula = For of symbol * (term list);;

type head = AF of atomic_formula;;
type body = AFL of atomic_formula list;;

type fact = H of head;;

type rule = HB of head * body;;

type clause = F of fact | R of rule;; 

type program = P of clause list;;

type goal = G of atomic_formula list;;

exception End_of_file

type 'a prBool = Yes of 'a| No;;

type substitution = (term * term) list ;;

exception NOT_UNIFIABLE;;

let rec print_term t = match t with
    |   V(Var(a))               ->  print_string"V(Var("; print_string a; print_string"))";  
    |   C(Cons(a))              ->  print_string"C(Cons("; print_string a; print_string"))";
    |   Func(Sym(a), b)         ->  print_string"Func(Sym(";print_string a; print_string")"; 
                                    let rec func t = match t with
                                        | []    ->  print_string ""
                                        | a::b  ->  print_term a; func b
                                    in
                                    func b; print_string")";
;;

let rec print_term_list f = match f with
    |   []      ->  print_string ""
    |   a::b    ->  print_term a; print_term_list b
;;    

exception Error;;
let print_atomic f = match f with
    |   For(Sym(a), b)    ->  print_string"For(Sym(";print_string a; print_string")"; print_term_list b; print_string")";
;;    

let rec print_body f = match f with
    |   []      ->  print_string ""
    |   a::b    ->  print_atomic a; print_body b
;;    

let print_head f = match f with
    |   AF(a)    ->  print_string"AF("; print_atomic a; print_string")";
;;    

let print_rule f = match f with
    |   HB(a, AFL(b))    ->  print_string"HB("; print_head a; print_string")B("; print_body b; print_string")";
;;    

let print_fact f = match f with
    |   H(a)    ->  print_string"H("; print_head a; print_string")";
;;    

let print_clause c = match c with
    |   F(a)    ->  print_string"F("; print_fact a; print_string")"
    |   R(a)    ->  print_string"R("; print_rule a; print_string")"
;;    

let rec print_clause_list c = match c with
    |   []      ->  print_string "])\n"
    |   a::b    ->  print_clause a; print_string";\n"; print_clause_list b
;;  
let print_program p = match p with
    |   P(a)    ->  print_string"P(Cl(["; print_clause_list a; print_string")"
;;

let print_goals p = match p with
    |   G(a)    ->  print_string"G("; print_body a; print_string")"


let rec print_subs s = match s with
    |   []  ->  print_string ""
    |   (a, b)::c    ->  print_term a; print_string"----"; print_term b; print_subs c;
;;

let print_prbool s = match s with
    |   No  ->  print_string "No"
    |   Yes(a)  -> match a with
                    |   []  ->  print_string ""
                    |   (a, b)::c    ->  print_term a; print_string"----"; print_term b; print_subs c;
;;

let rec pr_or f l = match l with
    |   []      ->  No
    |   a::b    ->  let t = f a in
                    let func t = match t with 
                        | No        ->  pr_or f b
                        | Yes(a)    ->  t
                    in
                    func t
;;

let rec pr_or f l = match l with
    |   []      ->  No
    |   a::b    ->  let t = f a in
                    let func t = match t with 
                        | No        ->  pr_or f b
                        | Yes(a)    ->  t
                    in
                    func t
;;

let rec pr_and f e l = match l with
    |   []      ->  Yes(e)
    |   a::b    ->  let t = f e a in
                    let func t = match t with 
                        | No        ->  No
                        | Yes(z)    ->  pr_and f z b
                    in
                    func t
;;

let retrieve (t : term) (subst : substitution) = 
    
    let rec h1 (x : term) (s : substitution) (ss : substitution) l = match s with
    |   []              ->  x
    |   (v, t)::b       ->  (*let b1 = List.exists(fun element -> v = element) l in*)
                            let b2 = List.exists(fun element -> t = element) l in 
                            if v = x && b2 = false then
                                match t with 
                                    |   V(m)    ->  h1 t ss ss (t :: l)
                                    |   _       ->  t
                            (*else if t = x && b1 = false then 
                                match v with 
                                    |   V(m)    ->  h1 v ss ss (v :: l)
                                    |   _       ->  v*)
                            else
                                h1 x b ss l
    in

    let rec help1 (x : term) (s : substitution) = match s with
    |   []                              ->  []
    |   (V(c), C(Cons(d)))::b           ->  if x = V(c) then
                                                C(Cons(d)) :: help1 x b
                                            else
                                                help1 x b 
    |   a::b                            ->  help1 x b
    in
    
    let h = help1 t subst in 
    if List.length h = 0 then
        match t with 
            | V(p)  ->  h1 t subst subst [t]
            | _     ->  t
    else
        match h with 
            |   []      ->  h1 t subst subst [t]
            |   a::b    ->  a

;;


(*let retrieve (t : term) (subst : substitution) = 
    
    let rec h1 (x : term) (s : substitution) (ss : substitution) l = match s with
    |   []              ->  x
    |   (v, t)::b       ->  let b1 = List.exists(fun element -> v = element) l in 
                            let b2 = List.exists(fun element -> t = element) l in 
                            if v = x && b2 = false then
                                match t with 
                                    |   V(m)    ->  h1 t ss ss (t :: l)
                                    |   _       ->  t
                            else if t = x && b1 = false then 
                                match v with 
                                    |   V(m)    ->  h1 v ss ss (v :: l)
                                    |   _       ->  v
                            else
                                h1 x b ss l
    in

    match t with 
        | V(p)  ->  h1 t subst subst [t]
        | _     ->  t
;;*)


let bind (t1 : term) (t2 : term) (subst : substitution) = match (t1, t2) with
    | V(x), _           ->  (t1, t2) :: subst 
    | _, V(x)           ->  (t2, t1) :: subst
    | _, _              ->  raise Error
;;


let replace (t1 : term list ) (s1 : substitution) = 

    let rec helper sigma t = match t with
        |V(a)           ->  let r = retrieve t sigma in
                            r
        |C(a)           ->  t
        |Func(a, b)     ->  let rec func l = match l with
                                |   []      ->  []
                                |   x::y    ->  helper sigma x :: func y;
                            in
                            let y = func b in
                            Func(a, y)                            
    in             

    (*print_string "May the force be with you";*)
    let rec help l = match l with
        |   []      ->  []
        |   a::b    ->  helper s1 a :: help b 
    in 

    help t1 
;;    


let copy (t : clause list) = 

    let rec help5 tl = match tl with 
        |   []      ->  []
        |   a::b    ->  match a with
                            |   C(x)             ->     C(x) :: help5 b 
                            |   V(Var(x))        ->     V(Var(x ^ "'")) :: help5 b
                            |   Func(x, y)       ->     Func(x, help5 y) :: help5 y
    in  

    let help3 af = match af with
        |  For(x, y)   ->  For(x, help5 y)
    in

    let rec help4 tttt = match tttt with
        |   []      ->  []
        |   a::b    ->  help3 a :: help4 b
    in

    let help2 ttt = match ttt with
        |   F(H(AF(x)))                     ->  F(H(AF(help3 x)))
        |   R(HB(AF(x), AFL(y)))            ->  R(HB(AF(help3 x), AFL(help4 y)))
    in

    let rec help tt = match tt with
        |   []      ->  []
        |   a::b    ->  help2 a :: help b
    in

    help t
;;

let copy_goal (t : atomic_formula list) = 

    let rec help5 tl = match tl with 
        |   []      ->  []
        |   a::b    ->  match a with
                            |   C(x)             ->     C(x) :: help5 b 
                            |   V(Var(x))        ->     V(Var(x ^ "-")) :: help5 b
                            |   Func(x, y)       ->     Func(x, help5 y) :: help5 y
    in  

    let help3 af = match af with
        |  For(x, y)   ->  For(x, help5 y)
    in

    let rec help4 tttt = match tttt with
        |   []      ->  []
        |   a::b    ->  help3 a :: help4 b
    in

    help4 t
;;


let rec mgu (t1, t2 : term * term) (subst : substitution) = 

    let r1 = retrieve t1 subst in
    let r2 = retrieve t2 subst in

    let rec help (x1 : term) (x2 : term) (s : substitution) = match (x1, x2) with
        |   C(a), C(b)                  ->  if a = b then
                                                Yes (s)
                                            else
                                                No
        |   V(a), _                     ->  let t = bind x1 x2 s in 
                                            Yes(t)
        |   _, V(a)                     ->  let t = bind x2 x1 s in 
                                            Yes(t)
        |   Func(a, b), Func(c, d)      ->  if a = c && List.length b = List.length d then
                                                (
                                                let rec func x y (z : substitution) = match (x, y) with
                                                    [], []                      ->  Yes(z)
                                                    |[], e::f | e::f, []        ->  No
                                                    |e::f, g::h                 ->  let t = mgu (e, g) z in
                                                                                    match t with
                                                                                    |   No      ->  No
                                                                                    |   Yes(d)  ->  let s1 = replace f d in
                                                                                                    let s2 = replace h d in
                                                                                                    func s1 s2 d
                                                in
                                                func b d s
                                                )
                                            else
                                                No 
        |   _, _                        ->  No
    in

    help r1 r2 subst
;;    

let solve_given_af a1 a2 s = match (a1, a2) with
    |   For(w, x), For(y, z)    ->  if w = y && List.length x = List.length z then
                                        (
                                        let rec func l m (ss : substitution) = match (l, m) with
                                            [], []                      ->  Yes(ss)
                                            |[], e::f | e::f, []        ->  No
                                            |e::f, g::h                 ->  let t = mgu (e, g) ss in
                                                                                match t with
                                                                                |   No      ->  No
                                                                                |   Yes(d)  ->  let s1 = replace f d in
                                                                                                let s2 = replace h d in
                                                                                                func s1 s2 d
                                        in
                                        func x z s
                                        )
                                    else
                                        No
;;


let solve_given_head (data, subst : clause list * substitution) (g : atomic_formula) (c : head) = match c with
    |   AF(a)    ->  solve_given_af g a subst
;;

let flag = ref 0 ;;
let fir = ref 0 ;;
let to_do = ref [] 
let doing = ref 0;;

let print_ans (s : substitution) = 

    let rec help t g = match t with
    | []       ->   print_string ""; flush stdout; g
    | a::b     ->   match a with
                        |(V(Var(x)), f)  ->     let fu o = match o with 
                                                    |   V(gg)            -> help b g  
                                                    |   Func(gg, ggg)    -> help b g
                                                    |   C(Cons(v))       -> let b1 = List.exists(fun element -> V(Var(x)) = element) !to_do in 
                                                                            if b1 then 
                                                                                (if ((List.length !to_do - 1) = !doing) then
                                                                                    (incr doing; flag := 1; print_string (x ^ " = " ^ v ^ " "); flush stdout; help b 1)
                                                                                else
                                                                                    (incr doing; flag := 1; print_string (x ^ " = " ^ v ^ "\n"); flush stdout; help b 1))
                                                                            else
                                                                                (help b 1)
                                                in
                                                fu (retrieve f s)
                        |(_, _)          ->  print_string ""; flush stdout; help b g
    in
    let y = help s 0 in 
    if y = 0 then
        Yes(s)
    else
        No
;;

(*let compose s1 s2 = 
    let rec help l1 l2 = match (l1, l2) with
        |   ([], [])                  ->    []
        |   ([], _)                   ->    l2
        |   (_, [])                   ->    l1
        |   ((a, b)::c, (d, e)::f)    ->    if a = d && b = e then
                                                (a, b) :: help c f
                                            
                                            else if a = d && b != e then
                                                (a, e) :: help c f

                                            else if a != d && b = e then
                                                (a, b) :: (d, e) :: help c f
                                            
                                            else
                                                (a, b) :: (d, e) :: help c f

    in  
    help s1 s2
;;*)

let rec solve_given_clause (data, subst : clause list * substitution) (g) (c : clause) = match g with
    |   []              ->  Yes(subst)
    |   g_head::g_tail  ->  match c with
                                |   F(H(a))             ->  let fu1 o = match o with
                                                                |AF(For(Sym(b), c))    -> AF(For(Sym(b), replace c subst))
                                                            in
                                                            let a = fu1 a in
                                                            let fu2 oo = match oo with
                                                                |For(Sym(b), c)         -> For(Sym(b), replace c subst)
                                                            in
                                                            let g_head = fu2 g_head in
                                                            let t = solve_given_head (data, subst) g_head a in
                                                            let fn y = match y with 
                                                                |   No ->  No
                                                                |   Yes(x)  -> solve_goals (data, x) (g_tail)
                                                            in 
                                                            fn t   
                                |   R(HB(a, AFL(b)))    ->  let fu1 o = match o with
                                                                |AF(For(Sym(bb), c))    -> AF(For(Sym(bb), replace c subst))
                                                            in
                                                            let a = fu1 a in
                                                            let fu2 oo = match oo with
                                                                |For(Sym(bb), c)         -> For(Sym(bb), replace c subst)
                                                            in
                                                            let g_head = fu2 g_head in
                                                            
                                                            let t = solve_given_head (data, subst) g_head a in                                                        
                                                            match t with 
                                                                |   No      ->  No
                                                                |   Yes(x)  ->  solve_goals (data, x) (b @ g_tail)

and
solve_goals (data, subst : clause list * substitution) (goals : atomic_formula list) = match goals with
    |[]     ->  doing := 0;
                if (!fir) != 0 then
                    (print_string " ? ";
                    flush stdout;
                    let y = read_line() in
                    if y = ";" then
                        (print_string "\n";
                        flush stdout;
                        print_ans subst
                        )
                    else
                        (print_string "\n";
                        flush stdout;
                        Yes(subst))
                    )
                else(
                    print_string "\n";
                    flush stdout;
                    fir := 1;
                    print_ans subst)

    |a::b   ->  let data1 = copy data in
                pr_or (solve_given_clause (data1, subst) (goals)) data1

;;


let rec find_var v = match v with 
    |   []      ->  []
    |   a::b    ->  match a with 
                        |   V(x)        ->  V(x) :: find_var b    
                        |   Func(x, y)  ->  find_var y @ find_var b
                        |   _           ->  find_var b
;;

let rec find_to_do af = match af with
    |   []              ->   []  
    |   For(a, b)::c    ->  find_var b @ find_to_do c

;;

let solve (prog : program) (go : goal) = match (prog, go) with
    |   P(a), G(b)    ->    (*let dd = copy_goal b in *)
                            to_do := find_to_do b;
                            (*print_term_list !to_do;*)
                            let y = solve_goals (a, []) b in
                            match y with 
                                | No        ->  if !flag = 0 then
                                                    (print_string "No\n";
                                                    flush stdout;)
                                                else    
                                                    (print_string "Yes\n";
                                                    flush stdout;)
                                | Yes(x)    ->  print_string "Yes\n";
                                                flush stdout;
;;
