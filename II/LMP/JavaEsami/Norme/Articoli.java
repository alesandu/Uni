package Norme;

import java.util.HashMap;

public class Articoli {
	private int numeroArticolo;
	private String introduzione;
	
	public int getNumeroArticolo() {
		return numeroArticolo;
	}
	public void setNumeroArticolo(int numeroArticolo) {
		this.numeroArticolo = numeroArticolo;
	}
	public String getIntroduzione() {
		return introduzione;
	}
	public void setIntroduzione(String introduzione) {
		this.introduzione = introduzione;
	}
	public HashMap<Integer, String> getCommi() {
		return commi;
	}
	public void setCommi(HashMap<Integer, String> commi) {
		this.commi = commi;
	}
	private HashMap<Integer, String> commi;

	public Articoli(int numeroArticolo, String introduzione, HashMap<Integer, String> commi) {
		this.numeroArticolo = numeroArticolo;
		this.introduzione = introduzione;
		this.commi = commi;
	}
	
}
