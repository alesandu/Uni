package esame1;

import java.time.LocalDate;

public class Prestito {
	
	private LocalDate dataInizio;
	private LocalDate dataRiconsegnaPrevista;
    private LocalDate dataRiconsegnaEffettiva;
    private String utente;
    private double costoAffitto;
    
    public Prestito(LocalDate dataInizio, LocalDate dataRiconsegnaPrevista,
			String utente, double costoBase) {
		this.dataInizio = dataInizio;
		this.dataRiconsegnaPrevista = dataRiconsegnaPrevista;
		this.dataRiconsegnaEffettiva = null;
		this.utente = utente;
		this.costoAffitto = costoBase;
	}

	public Prestito(LocalDate dataInizio, LocalDate dataRiconsegnaPrevista, LocalDate dataRiconsegnaEffettiva,
			String utente, double costoAffitto) {
		this.dataInizio = dataInizio;
		this.dataRiconsegnaPrevista = dataRiconsegnaPrevista;
		this.dataRiconsegnaEffettiva = dataRiconsegnaEffettiva;
		this.utente = utente;
		setCostoAffitto(costoAffitto);
	}
    
    public LocalDate getDataInizio() {
		return dataInizio;
	}
	public void setDataInizio(LocalDate dataInizio) {
		this.dataInizio = dataInizio;
	}
	public LocalDate getDataRiconsegnaPrevista() {
		return dataRiconsegnaPrevista;
	}
	public void setDataRiconsegnaPrevista(LocalDate dataRiconsegnaPrevista) {
		this.dataRiconsegnaPrevista = dataRiconsegnaPrevista;
	}
	public LocalDate getDataRiconsegnaEffettiva() {
		return dataRiconsegnaEffettiva;
	}
	public void setDataRiconsegnaEffettiva(LocalDate dataRiconsegnaEffettiva) {
		this.dataRiconsegnaEffettiva = dataRiconsegnaEffettiva;
	}
	public String getUtente() {
		return utente;
	}
	public void setUtente(String utente) {
		this.utente = utente;
	}
	public double getCostoAffitto() {
		return costoAffitto;
	}
	
	public int calcolaDurata() {
		if(dataRiconsegnaEffettiva != null)
			return dataInizio.until(dataRiconsegnaEffettiva).getDays();
		else
			return 0;
    }
	
	public void setCostoAffitto(double costoAffitto) {
		if(dataRiconsegnaEffettiva.isAfter(dataRiconsegnaPrevista) && dataRiconsegnaEffettiva!=null) {
			this.costoAffitto = costoAffitto + 10;
		}
		else
			this.costoAffitto = costoAffitto;
	}
	
	public void setGiorniRiconsegna(LocalDate dataRiconsegna) {
        if(dataRiconsegnaEffettiva.isAfter(dataRiconsegnaPrevista)) {
			this.costoAffitto = costoAffitto - 10;
		}
        this.dataRiconsegnaPrevista = dataRiconsegna;
    }
	
}
