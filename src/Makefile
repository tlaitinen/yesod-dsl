all:
	ghc --make main.hs -o yesod-generate-rest


clean:
	rm -f *.o *.hi yesod-generate-rest Parser.hs Lexer.hs
	happy -g Parser.y
	alex -g Lexer.x



