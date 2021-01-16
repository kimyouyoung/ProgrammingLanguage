package edu.handong.csee.plt.ast;

public class Seqn extends AST{
	
	AST first = new AST();
	AST second = new AST();
	
	public Seqn(AST first, AST second) {
		this.first = first;
		this.second = second;
	}
	
	public AST getFirst() {
		return first;
	}

	public AST getSecond() {
		return second;
	}

	public String getASTCode() {
		return "(seqn " + first.getASTCode() + " " + second.getASTCode() + ")";
	}
	
}
