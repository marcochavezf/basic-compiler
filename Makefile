basic.exe : BasicLexer.fs BasicParser.fs BasicAST.fs BasicUtils.fs BasicSemantic.fs BasicCodeGenerator.fs BasicMain.fs basiclib.dll
	fsc --nologo -r FSharp.PowerPack.dll BasicAST.fs BasicParser.fs BasicLexer.fs BasicUtils.fs BasicSemantic.fs BasicCodeGenerator.fs BasicMain.fs --out:basic.exe

BasicLexer.fs : BasicLexer.fsl
	fslex BasicLexer.fsl --unicode
	
BasicParser.fs : BasicParser.fsy
	fsyacc BasicParser.fsy --module BasicParser
	
basiclib.dll: basiclib.cs
	dmcs /t:library basiclib.cs
	
clean :
	rm BasicLexer.fs BasicParser.fs BasicParser.fsi basic.exe basiclib.dll  prueba1.il prueba1.exe prueba2.il prueba2.exe prueba3.il prueba3.exe prueba4.il prueba4.exe output.il output.exe
