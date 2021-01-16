package edu.handong.csee.plt.ast;

public class App extends AST{
	
	AST ftn = new AST();
	AST arg = new AST();
	
	public App(AST ftn, AST arg) {
		this.ftn = ftn;
		this.arg = arg;
	}
	
	public AST getFtn() {
		return ftn;
	}
	
	public AST getArg() {
		return arg;
	}

	public String getASTCode() {
		return "(app " + ftn.getASTCode() + " " + arg.getASTCode() + ")";
	}
}
