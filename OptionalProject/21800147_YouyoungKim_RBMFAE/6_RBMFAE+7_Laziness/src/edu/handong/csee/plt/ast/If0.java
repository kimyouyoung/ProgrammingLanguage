package edu.handong.csee.plt.ast;

public class If0 extends AST{
	
	AST test = new AST();
	AST then = new AST();
	AST else_t = new AST();
	
	public If0(AST test, AST then, AST else_t) {
		this.test = test;
		this.then = then;
		this.else_t = else_t;
	}
	
	public AST getTest() {
		return test;
	}

	public AST getThen() {
		return then;
	}
	
	public AST getElse() {
		return else_t;
	}

	public String getASTCode() {
		return "(if0 " + test.getASTCode() + " " + then.getASTCode() + " " + else_t.getASTCode() + ")";
	}
}
