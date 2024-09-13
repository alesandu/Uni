package ImpresaSportiva;

import java.util.ArrayList;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;

public class Impresa {
	private ArrayList<Sportivi> sportivi = new ArrayList<Sportivi>();

	public ArrayList<Sportivi> getSportivi() {
		return sportivi;
	}

	public void setSportivi(ArrayList<Sportivi> sportivi) {
		this.sportivi = sportivi;
	}

	public Impresa(ArrayList<Sportivi> sportivi) {
		super();
		this.sportivi = sportivi;
	}
	
	public HashMap<Sportivi,Integer> topPlayer(ArrayList<Sportivi> sportivi, int N){
		HashMap<Sportivi,Integer> temp = null;
		for(Sportivi x : sportivi) {
			int contatoreReti = 0;
			int contatoreMesi = 0;
			for(int mese: x.getReti().keySet()) {
				if(x.getReti().get(mese) > 0) {
					contatoreReti += x.getReti().get(mese);
					contatoreMesi++;
				}
			}
			if((x.getLivelloStipendio() >= 3) && (contatoreReti>=N) && (x.getTipo() == Tipologia.GIOCATORE)){
				temp.put(x,contatoreMesi);
			}
		}
		return temp;
	}
	
	public HashMap<String,ArrayList<Sportivi>> doppio(ArrayList<Sportivi> sportivi) {
		HashMap<String,ArrayList<Sportivi>> lista = null;
		for(Sportivi x : sportivi) {
			DateTimeFormatter y = DateTimeFormatter.ofPattern("mm-yy");
			String meseAnno = x.getData().format(y);
			if(lista.containsValue(meseAnno)) {
				lista.get(meseAnno).add(x);
				}
			else {
				lista.put(meseAnno, null);
				lista.get(meseAnno).add(x);
				}
		}
		for(String chiave: lista.keySet()) {
			if(lista.get(chiave).size()==1) {
				lista.remove(chiave);
			}
		}
		return lista;
		
	}
}
