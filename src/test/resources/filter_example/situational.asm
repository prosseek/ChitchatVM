    f2 nearCityPark 0
END:
    stop
nearCityPark:
    read latitude
    jmpnull END
    read longitude
    jmpnull END
    push [30, 25, 1, 74]
    push [-97, 47, 21, 83]
    abs location
    push 5000.0
    fleq
    r 0
