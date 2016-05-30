# correlation producePrice = (priceMatch(produceName, price))
# function bool priceMatch(produceName, price) = {
#  return ( produceName == "apple" && 0 <= price <= 1000 )
# }

# first
    read producename
    jpeekfalse END
    read price_i
    jpeekfalse END
    function_call_stack priceMatch 2
END:
    stop

priceMatch:
    load $bp - 2
    push "apple"
    cmp

    push 0
    push 1000
    load $bp - 1
    inrange 0

    and 2
    return 2