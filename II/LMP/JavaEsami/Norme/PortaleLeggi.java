package Norme;

import java.time.LocalDate;
import java.util.ArrayList;

public class PortaleLeggi {
	
	private ArrayList<Leggi> leggi;

	public ArrayList<Leggi> getLeggi() {
		return leggi;
	}

	public void setLeggi(ArrayList<Leggi> leggi) {
		this.leggi = leggi;
	}

	public PortaleLeggi(ArrayList<Leggi> leggi) {
		super();
		this.leggi = leggi;
	}
	
	public Leggi recuperaLegge(TipoLegge tipo, LocalDate data) {
		Leggi temp = null;
		for(Leggi x : leggi) {
			if(x.getData() == data && x.getTipologia() == tipo) {
				temp = x;
			}
		}
		return temp;
	}

	@Override
	public String toString() {
		return " " + leggi;
	}
	
	public ArrayList<Leggi> mostraLeggi(){
		ArrayList<Leggi> ris = new ArrayList<>();
		for(Leggi x : leggi) {
			if(x.getArticolata().size() > 20) {
				ris.add(x);
			}
		}
		return ris;
		
	}
	
	
}
