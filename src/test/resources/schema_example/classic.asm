# (name , event | advertisement , time?)

    read name
    jpeekfalse END
    register name

    read event
    jpeekfalse TRYALIAS1
    register event
    jmp NEXT1
TRYALIAS1:
    pop temp
    read advertisement
    jpeekfalse END
    register advertisement
NEXT1:
    read time
    jpeekfalse NEXT2
    register time
    jmp NEXT3
NEXT2:
    pop temp
NEXT3:
# final - summing up the summary
    push_summary
END:
    stop
