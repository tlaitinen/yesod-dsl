all: yesod-generate-rest

yesod-generate-rest: main.hs Lexer.hs Parser.hs ModuleMerger.hs ClassImplementer.hs AST.hs Generator.hs SyncFiles.hs CheckFieldNames.hs CheckServices.hs
	ghc --make main.hs -o yesod-generate-rest

Lexer.hs: Lexer.x
	alex -g Lexer.x

Parser.hs: Parser.y
	happy -g Parser.y

clean:
	rm -f *.o *.hi yesod-generate-rest
