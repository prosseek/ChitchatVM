# first
    read producename
    jmpnull END
    read price_i
    jmpnull END
    function_call_stack priceMatch 2
END:
    stop

priceMatch:
    load bp - 2
    push "apple"
    cmp

    push 0
    push 1000
    load bp - 1
    inrange 0

    and 2
    return 2