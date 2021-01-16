package edu.handong.csee.plt.ast;

public class AST {
	
	public String getASTCode() {
		
		String astCode="";
		
		if(this instanceof Add)
			astCode = ((Add)this).getASTCode();
		
		if(this instanceof Sub)
			astCode = ((Sub)this).getASTCode();
		
		if(this instanceof Id)
			astCode = ((Id)this).getASTCode();
		
		if(this instanceof Num)
			astCode = ((Num)this).getASTCode();
		
		if(this instanceof Fun)
			astCode = ((Fun)this).getASTCode();
		
		if(this instanceof App)
			astCode = ((App)this).getASTCode();
		
		if(this instanceof With)
			astCode = ((With)this).getASTCode();
		
		if(this instanceof If0)
			astCode = ((If0)this).getASTCode();
		
		if(this instanceof Rec)
			astCode = ((Rec)this).getASTCode();

		return astCode;
	}
}

