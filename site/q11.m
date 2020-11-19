queens 0 = [[]]
queens (n+1) = [q:b|b<-queens n;q<-[1..11];safe q b]
safe q b = and[~checks q b i|i<-index b]
checks q b i = q=b!i \/ abs(q-b!i)=i+1
main = queens 11
