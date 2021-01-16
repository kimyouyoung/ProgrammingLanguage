package edu.handong.csee.plt;

import java.util.Vector;

import edu.handong.csee.plt.ast.*;
import edu.handong.csee.plt.defsub.*;
import edu.handong.csee.plt.store.*;
import edu.handong.csee.plt.val.*;

public class Interpreter {
	

	public ValueStore interp(AST ast, DefrdSub ds, Store sto) {
		
		if(ast instanceof Num) {
			return new ValueStore(new numV(((Num) ast).getStrNum()), sto);
		}
		
		if(ast instanceof Add) {
			Add add = (Add)ast;
			NumOp numop = new NumOp();
			Strict strict = new Strict();
			
			ValueStore lvs = strict.strict(interp(add.getLhs(), ds, sto));
			ValueStore rvs = strict.strict(interp(add.getRhs(), ds, lvs.getStore()));
			
			String num = numop.NumOperator("+", lvs.getValue().getValueCode(), rvs.getValue().getValueCode());
			
			return new ValueStore(new numV(num), rvs.getStore());
		}
		
		if(ast instanceof Sub) {
			Sub sub = (Sub)ast;
			NumOp numop = new NumOp();
			Strict strict = new Strict();
			
			ValueStore lvs = strict.strict(interp(sub.getLhs(), ds, sto));
			ValueStore rvs = strict.strict(interp(sub.getRhs(), ds, lvs.getStore()));
			
			String num = numop.NumOperator("-", lvs.getValue().getValueCode(), rvs.getValue().getValueCode());
			
			return new ValueStore(new numV(num), rvs.getStore());
		}
		
		if(ast instanceof Id) {
			Id id = (Id)ast;
			LookUp lookup = new LookUp();
			StoreLookUp slookup = new StoreLookUp();
			
			return new ValueStore(slookup.storeLookUp(lookup.lookUp(id.getName(), ds), sto), sto);
		}
		
		if(ast instanceof Fun) {
			Fun fun = (Fun)ast;
			return new ValueStore(new ClosureV(fun.getParam(), fun.getBody(), ds), sto);
		}
		
		if(ast instanceof ReFun) {
			ReFun refun = (ReFun)ast;
			return new ValueStore(new refclosV(refun.getParam(), refun.getBody(), ds), sto);
		}

		if(ast instanceof If0) {
			If0 if0 = (If0)ast;
			
			if(numZero((interp(if0.getTest(), ds, sto).getValue())))
				return interp(if0.getThen(), ds, sto);
			
			else
				return interp(if0.getElse(), ds, sto);
			
		}

		if(ast instanceof Rec) {
			Rec rec = (Rec)ast;
			Vector<Value> v_holder = new Vector<Value>();
			aRecSub new_ds = new aRecSub(rec.getName(), v_holder, ds);
			
			v_holder.add((interp(rec.getExp(), new_ds, sto)).getValue());
			
			return interp(rec.getFcall(), new_ds, sto);
		}
		
		if(ast instanceof App) {
			App app = (App)ast;
			Malloc malloc = new Malloc();
			Strict str = new Strict();
			ValueStore vs = str.strict(interp(app.getFtn(), ds, sto));
			Value value = vs.getValue();
			
			if(app.getArg() instanceof NewBox) {
				ValueStore a = interp(app.getArg(), ds, vs.getStore());
				int new_address = malloc.malloc(a.getStore());
				
				ClosureV fval = (ClosureV)value;
				
				return interp(fval.getBody(), new aSub(fval.getParam(), new_address, fval.getDefrdSub()), new aSto(new_address, a.getValue(), a.getStore()));
			}
			else {
				
				if(value instanceof ClosureV) {
					ClosureV fval = (ClosureV)value;
					
					int new_address = malloc.malloc(vs.getStore());
					ValueStore a = interp(app.getArg(), ds, vs.getStore());
					
					return interp(fval.getBody(), new aSub(fval.getParam(), new_address, fval.getDefrdSub()), new aSto(new_address, a.getValue(), a.getStore()));
				}
		
				else if(value instanceof refclosV) {
					
					refclosV ref = (refclosV)value;
					LookUp lookup = new LookUp();
					Id id = (Id)app.getArg();
					
					int address = lookup.lookUp(id.getName(), ds);
					
					return interp(ref.getBody(), new aSub(ref.getParam(), address, ref.getDefrdSub()), vs.getStore());
				}
			
			}
		}
		
		if(ast instanceof NewBox) {
			NewBox newbox = (NewBox)ast;
			Malloc malloc = new Malloc();
			
			
			int new_address = malloc.malloc(sto);
			Vector<Value> v = new Vector<Value>();
			v.add(new First());
			exprV aval = new exprV(newbox.getVal(), ds, sto, v);
			
			return new ValueStore(new boxV(new_address), new aSto(new_address, aval, sto));
		}
		
		if(ast instanceof SetBox) {
			SetBox setbox = (SetBox)ast;
			Strict strict = new Strict();
			
			ValueStore vs = strict.strict(interp(setbox.getBn(), ds, sto));
			boxV boxv = (boxV)vs.getValue();
			ValueStore vs2 = strict.strict(interp(setbox.getVal(), ds, vs.getStore()));
			
			return new ValueStore(vs2.getValue(), new aSto(boxv.getAddress(), vs2.getValue(), vs2.getStore()));
		}
                
		if(ast instanceof OpenBox) {
			OpenBox openbox = (OpenBox)ast;
			StoreLookUp slookup = new StoreLookUp();
			Strict strict = new Strict();
			
			ValueStore vs = interp(openbox.getVal(), ds, sto);
			boxV boxv = (boxV)vs.getValue();
			ValueStore re_val = strict.strict(new ValueStore(slookup.storeLookUp(boxv.getAddress(), vs.getStore()), sto));
			
			if(re_val.equals(vs))
				return new ValueStore(re_val.getValue(), vs.getStore());
			
			else
			 return new ValueStore(re_val.getValue(), sto);
		}
		
		if(ast instanceof SetVar) {
			SetVar setvar = (SetVar)ast;
			LookUp lookup = new LookUp();
			
			int address = lookup.lookUp(setvar.getId(), ds);
			
			ValueStore vs = interp(setvar.getUpdate(), ds, sto);
			
			return new ValueStore(vs.getValue(), new aSto(address, vs.getValue(), vs.getStore()));
		}
		
		if(ast instanceof Seqn) {
			Seqn seqn = (Seqn)ast;
			Strict strict = new Strict();
			
			ValueStore vs = strict.strict(interp(seqn.getFirst(), ds, sto));
			ValueStore vs2 = strict.strict(interp(seqn.getSecond(), ds, vs.getStore()));
			
			return new ValueStore(vs2.getValue(), vs2.getStore());
			
		}
		
		return null;
	}
	
	public boolean numZero(Value v) {
		
		String num = v.getValueCode();
		num = num.replaceAll("[^0-9]", "");
		
		if(num.equals("0"))
			return true;
		
		return false;
	}
}
