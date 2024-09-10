package p170624;
import java.util.ArrayList;
import java.util.HashMap;

public class Manager {
	
	private ArrayList<Prodotti> catena;
	
	public Manager(ArrayList<Prodotti> catena) {
		this.catena = catena;
	}

	public void addProdotto(ArrayList<Prodotti> prodotti) {
		catena.addAll(prodotti);
	}
	
	public void addProdotto(Prodotti prodotto) {
		catena.add(prodotto);
	}
	
	public void setFattoreGuadagno(double fattoreGuadagno) {
		for(Prodotti prodotti : catena) {
			prodotti.setFattoreGuadagno(fattoreGuadagno);
			prodotti.setPrezzoA();
		}
	}
	
	public void setCostoManodopera(double costoGiornaliero) {
		for(Prodotti prodotti : catena) {
			prodotti.setCostoGiornaliero(costoGiornaliero);
			prodotti.setCostoManodopera();
		}
	}
	
	public HashMap<Double, Prodotti> getRanking(){
		HashMap<Double, Prodotti> rank = new HashMap<Double, Prodotti>();
		for(Prodotti prodotti : catena) {
			double guadagno = prodotti.getPrezzoA()-(prodotti.getCostoManodopera()+prodotti.getCostoProduzione());
			Double guadagnoTempo = guadagno/prodotti.getTempoAssemblaggio();
			rank.put(guadagnoTempo,prodotti);
		}
		return rank;
	}
	
	public void presentazione() {
		for(Prodotti prodotti : catena) {
			prodotti.toString();
		}
	}
	
}
