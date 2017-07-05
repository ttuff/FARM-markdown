library(FARM)
a <- TheOriginOfSpecies(5, 1)
NewTip(a, 1, 2, 1)
plot(makePhy(a))
NewTip(a, 2, 3, 1)
plot(makePhy(a))
NewTip(a, 3, 4, 1)
plot(makePhy(a))
NewTip(a, 4, 5, 1)
plot(makePhy(a))

DropTip(a, 5)
plot(makePhy(a))

DropTip(a, 4)
plot(makePhy(a))

DropTip(a, 3)
plot(makePhy(a))

