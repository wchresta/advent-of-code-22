import Data.Char (ord)
main=process.lines=<<getContents
process=print.sum.map(score.evalPattern.parse)
parse(a:_:[b])=(ord a-65,ord b-89)
evalPattern(m,r)=(m,mod(m+r)3)
score(a,b)=b+1+3*mod(b-a+1)3
