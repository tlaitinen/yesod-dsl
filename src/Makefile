all:
	ghc --make main.hs -o yesod-generate-rest

Lexer.hs: Lexer.x
	alex -g Lexer.x

Parser.hs: Parser.y
	happy -g Parser.y

clean:
	rm -f *.o *.hi yesod-generate-rest
