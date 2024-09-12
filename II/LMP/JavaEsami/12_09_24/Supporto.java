package esame1;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

public class Supporto {
	private String titolo;
    private String entePubblicante;
    private int annoPubblicazione;
    private ArrayList<Prestito> prestiti;
    
    public Supporto(String titolo, String entePubblicante, int annoPubblicazione, ArrayList<Prestito> prestiti) {
		this.titolo = titolo;
		this.entePubblicante = entePubblicante;
		this.annoPubblicazione = annoPubblicazione;
		this.prestiti = prestiti;
	}
	
    public String getTitolo() {
		return titolo;
	}
	public void setTitolo(String titolo) {
		this.titolo = titolo;
	}
	public String getEntePubblicante() {
		return entePubblicante;
	}
	public void setEntePubblicante(String entePubblicante) {
		this.entePubblicante = entePubblicante;
	}
	public int getAnnoPubblicazione() {
		return annoPubblicazione;
	}
	public void setAnnoPubblicazione(int annoPubblicazione) {
		this.annoPubblicazione = annoPubblicazione;
	}
	public ArrayList<Prestito> getPrestiti() {
		return prestiti;
	}
	public void setPrestiti(ArrayList<Prestito> prestiti) {
		this.prestiti = prestiti;
	}
	
	public int calcolaPeriodoPiuLungoPrestito() {
        int periodoPiuLungo = 0;
        for (Prestito prestito : prestiti) {
            int periodo = prestito.calcolaDurata();
            if (periodo > periodoPiuLungo) {
                periodoPiuLungo = periodo;
            }
        }
        return periodoPiuLungo;
    }
	
	public ArrayList<String> verificaIncongruenze() {
		ArrayList<String> incongruenze = new ArrayList<String>();
		Collections.sort(prestiti, Comparator.comparing(Prestito::getDataInizio));
		for (int i = 0; i < prestiti.size() - 1; i++) {
	        Prestito attuale = prestiti.get(i);
	        Prestito successivo = prestiti.get(i + 1);
	        LocalDate dataFine = successivo.getDataInizio();
	        if(attuale.getDataRiconsegnaEffettiva() != null) {
	        	dataFine = attuale.getDataRiconsegnaEffettiva();
	        }
	        if (successivo.getDataInizio().isBefore(dataFine)) {
	            incongruenze.add("Incongruenza tra il prestito di " + attuale.getUtente() + " e " + successivo.getUtente());
	        }
	    }
		return incongruenze;
	}	
    
}

