package edu.handong.csee.plt;

import edu.handong.csee.plt.ast.*;
import edu.handong.csee.plt.defsub.*;
import edu.handong.csee.plt.store.*;
import edu.handong.csee.plt.val.*;

public class Main {
	
	static boolean onlyParser = false; // for -p option
	static boolean both = false;
	
	public static void main(String[] args) {
		
		// This is just an example code. Use args to get -p option and actuall code from CLI
		String str;
		
		if(args[0].equals("-p")) {
			onlyParser = true;
			str = args[1];
		}
		else if(args[0].equals("-b")) {
			both = true;
			str = args[1];
		}
		else
			str = args[0];
		
		// Parser
		Parser parser = new Parser();
		AST ast = parser.parse(str);
		
		if(ast == null) {
			System.out.println("Syntax Error!");
			System.exit(0);
		}
		
		if(onlyParser || both)
			System.out.println(ast.getASTCode());
		
		if(!onlyParser) {
			// interpreter
			Interpreter interpreter = new Interpreter();
			
			ValueStore result = interpreter.interp(ast, new mtSub(), new mtSto());
			
			if(result != null)
				System.out.println(result.getValueStoreCode());
		}
	}
}
