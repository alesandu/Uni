package ImpresaSportiva;

import java.util.ArrayList;
import java.time.LocalDate;
import java.util.HashMap;

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
	
	public ArrayList<Sportivi> topPlayer(ArrayList<Sportivi> sportivi, int N){
		ArrayList<Sportivi> temp = null;
		for(Sportivi x : sportivi) {
			int contatore = 0;
			for(int gol: x.getReti().values()) {
				contatore += gol;
			}
			if((x.getLivelloStipendio() > 3) && contatore>=N){
				temp.add(x);
			}
		}
		return temp;
	}
	
	public HashMap<LocalDate,ArrayList<Sportivi>> doppio(ArrayList<Sportivi> sportivi) {
		HashMap<LocalDate,ArrayList<Sportivi>> lista = null;
		for(Sportivi x : sportivi) {
			ArrayList<Sportivi> gemelli = null;
			for(Sportivi y : sportivi) {
				Tipologia tipoA = x.getTipo();
				Tipologia tipoB = y.getTipo();
				int meseA = x.getData().getMonthValue();
				int meseB = y.getData().getMonthValue();
				int annoA = x.getData().getYear();
				int annoB = y.getData().getYear();
				boolean check = ((tipoA == tipoB) && (meseA == meseB) && (annoA == annoB));
				if(check) {
					gemelli.add(x);
				}
			}
			lista.put(x.getData(), gemelli);
		}
		return lista;
		
	}
}
