    get producename
    get price
    function_call priceMatch producename price

    stop

priceMatch:
    load bp - 2
    push "apple"
    cmp

    push 0
    push 1000
    load bp - 1
    inrange0

    and
    return 2