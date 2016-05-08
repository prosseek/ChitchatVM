# (name , event , (sensor, value)+ )

    read name
    jpeekfalse END
    register name

    read event
    jpeekfalse END
    register event

    #print "OK0"
    f ITER
    push_summary
END:
    stop

ITER:
    #print "OK1"
    # i = 0
    # one local variable
    # i == bp + 2 -> first variable
    push 0

    # assignment i = 0
    push 0
    store bp + 2

    # there should be at least one sensor data
    load bp + 2
    pop temp
    read sensor temp
    jpeekfalse END
    register sensor temp
    read value temp
    jpeekfalse END
    register value temp

START:
    # while (true)
    #   i += 1
    #   read sensor i
    #   read value  i

    # i = i + 1
    load bp + 2
    push 1
    iadd
    store bp + 2
    load bp + 2
    pop temp

    read sensor temp
    jfalse ENDLOOP
    register sensor temp
    read value temp
    jfalse ENDLOOP
    register value temp

    jmp START
ENDLOOP:
    # remove two local variables
    pop
    r 0