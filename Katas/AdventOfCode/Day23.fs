module Day23

    type Reg = A | B

    type Offset = Plus of int | Minus of int

    type Inst =
        | HLF of Reg // Sets register r to half its current value, then continues with the next instruction.
        | TPL of Reg // Sets register r to triple its current value, then continues with the next instruction.
        | INC of Reg // Increments register r, adding 1 to it, then continues with the next instruction.
        | JMP of Offset // Is a jump; it continues with the instruction offset away relative to itself.
        | JIE of Reg * Offset // Offset is like jmp, but only jumps if register r is even ("jump if even").
        | JIO of Reg * Offset // Offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd).

    let isOne = (=) 1

    let isEven x = (x % 2) = 0

    let half' x = x / 2

    let triple' = (*) 3

    let incr' = (+) 1

    let oper (fn: int -> int) reg a b pos =
        match reg with
        | A -> fn a, b, pos + 1
        | B -> a, fn b, pos + 1

    let jump offset a b pos =
        match offset with
        | Plus x -> a, b, pos + x
        | Minus x -> a, b, pos - x

    let jumpfn (fn: int -> bool) offset reg a b pos =
        let x = match reg with A -> a | B -> b
        if fn x then (jump offset a b pos) else a, b, pos + 1

    let half = oper half'

    let triple = oper triple'

    let incr = oper incr'

    let jie = jumpfn isEven

    let jio = jumpfn isOne

    let eval inst a b pos =
        match inst with
        | HLF reg -> half reg a b pos
        | TPL reg -> triple reg a b pos
        | INC reg -> incr reg a b pos
        | JMP offset -> jump offset a b pos
        | JIE (reg, offset) -> jie offset reg a b pos
        | JIO (reg, offset) -> jio offset reg a b pos

    let rec run (program: Inst[]) a b pos =
        match (Array.tryItem pos program) with
        | Some inst ->
            let (a', b', pos') = eval inst a b pos
            run program a' b' pos'
        | _ -> a, b

    let instructions = [|
        INC A;
        JIO (A, Plus 2);
        TPL A;
        INC A
    |]

    let instructions' = [|
        JIO (A, Plus 22);
        INC A;
        TPL A;
        TPL A;
        TPL A;
        INC A;
        TPL A;
        INC A;
        TPL A;
        INC A;
        INC A;
        TPL A;
        INC A;
        INC A;
        TPL A;
        INC A;
        INC A;
        TPL A;
        INC A;
        INC A;
        TPL A;
        JMP (Plus 19);
        TPL A;
        TPL A;
        TPL A;
        TPL A;
        INC A;
        INC A;
        TPL A;
        INC A;
        TPL A;
        INC A;
        INC A;
        TPL A;
        INC A;
        INC A;
        TPL A;
        INC A;
        TPL A;
        TPL A;
        JIO (A, Plus 8);
        INC B;
        JIE (A, Plus 4);
        TPL A;
        INC A;
        JMP (Plus 2);
        HLF A;
        JMP (Minus 7)
    |]

    let test =
        //Puzzle 1 => A: 1 B: 255
        //Puzzle 2 => A: 1 B: 334

        let (x, y) = run instructions' 0 0 0
        printfn "Puzzle 1 => A: %i B: %i" x y

        let (x', y') = run instructions' 1 0 0
        printfn "Puzzle 2 => A: %i B: %i" x' y'