# -schema datetime = (date, time)
# -schema location = (longitude, latitude)
# -schema event = event | advertisement
# -schema sender = (name, id?) | "gchat id"
# +schema buyQuery = (sender, event, datetime, location)

    function_call BUYQUERY
    stop

# +schema buyQuery = (sender, event, datetime, location)
BUYQUERY:
    f SENDER
    jpeekfalse BUYQUERYEND

    f EVENT
    jpeekfalse BUYQUERYEND

    f DATETIME
    jpeekfalse BUYQUERYEND

    f LOCATION
    jpeekfalse BUYQUERYEND

    push_summary
BUYQUERYEND:
    r 0

# -schema sender = (name, id?) | ("gchat id")
SENDER:
    read name
    jpeekfalse SENDEREND
    register name

    read id
    jpeekfalse SENDEREND
    register id
SENDEREND:
    r 0