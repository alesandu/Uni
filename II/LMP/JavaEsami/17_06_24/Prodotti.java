package p170624;

import java.util.ArrayList;

public class Prodotti{

    private String identificativo;
    private String etichetta;
    private double costoProduzione;
    private double prezzoA;
    private ArrayList<Componenti> subcomponenti;
    private String paeseProvenienza;
    private int tempoRealizzazione;
    private double fattoreGuadagno;
    private double costoGiornaliero;
    private double costoManodopera;
    
    
    
	public Prodotti(String identificativo, String etichetta,
			ArrayList<Componenti> subcomponenti, String paeseProvenienza, int tempoRealizzazione,
			double fattoreGuadagno, double costoGiornaliero) {
		this.identificativo = identificativo;
		this.etichetta = etichetta;
		this.subcomponenti = subcomponenti;
		this.paeseProvenienza = paeseProvenienza;
		this.tempoRealizzazione = tempoRealizzazione;
		this.fattoreGuadagno = fattoreGuadagno;
		this.costoGiornaliero = costoGiornaliero;
		costoManodopera = getTempoAssemblaggio()*costoGiornaliero;
		costoProduzione = costoManodopera + getCostoComponenti();
		prezzoA = (costoManodopera+costoProduzione)*fattoreGuadagno;
	}

	public double getCostoManodopera() {
		return costoManodopera;
	}
	
	public void setCostoManodopera() {
		this.costoManodopera = getTempoAssemblaggio()*costoGiornaliero;
	}
	
	public double getCostoGiornaliero() {
		return costoGiornaliero;
	}

	public void setCostoGiornaliero(double costoGiornaliero) {
		this.costoGiornaliero = costoGiornaliero;
	}

	public void setCostoManodopera(double costoManodopera) {
		this.costoManodopera = costoManodopera;
	}

	public int getTempoOrdine() {
		int durata = 0;
		for(Componenti subcomponente : subcomponenti) {
			if(subcomponente.getTempiOrdinazione() > durata) {
				durata = subcomponente.getTempiOrdinazione();
			}
		}
		return durata;
	}
	
	public int getTempoAssemblaggio() {
		int durata = getTempoOrdine();
		durata += tempoRealizzazione;
		return durata;
	}

	public double getCostoComponenti() {
		double costoComplessivo = 0;
		for(Componenti subcomponente : subcomponenti) {
			costoComplessivo += subcomponente.getCosto();
		}
		return costoComplessivo;
	}
	
	public void setCostoProduzione() {
		double costoComp = getCostoComponenti();
		double costoProduzione =  costoManodopera + costoComp;
		this.costoProduzione = costoProduzione;
	}

	public void setPrezzoA() {
		double temp = getCostoProduzione() + getCostoManodopera();
		this.prezzoA =  temp*fattoreGuadagno;
	}

	public void setCostoProduzione(double costoProduzione) {
		this.costoProduzione = costoProduzione;
	}

	public void setPrezzoA(double prezzoA) {
		this.prezzoA = prezzoA;
	}

	public double getCostoProduzione() {
		return costoProduzione;
	}

	public String getIdentificativo() {
		return identificativo;
	}

	public void setIdentificativo(String identificativo) {
		this.identificativo = identificativo;
	}

	public String getEtichetta() {
		return etichetta;
	}

	public void setEtichetta(String etichetta) {
		this.etichetta = etichetta;
	}

	public double getPrezzoA() {
		return prezzoA;
	}

	public ArrayList<Componenti> getSubcomponenti() {
		return subcomponenti;
	}

	public void setSubcomponenti(ArrayList<Componenti> subcomponenti) {
		this.subcomponenti = subcomponenti;
	}

	public String getPaeseProvenienza() {
		return paeseProvenienza;
	}

	public void setPaeseProvenienza(String paeseProvenienza) {
		this.paeseProvenienza = paeseProvenienza;
	}

	public int getTempoRealizzazione() {
		return tempoRealizzazione;
	}

	public void setTempoRealizzazione(int tempoRealizzazione) {
		this.tempoRealizzazione = tempoRealizzazione;
	}

	public double getFattoreGuadagno() {
		return fattoreGuadagno;
	}
	
	public void setFattoreGuadagno(double fattoreGuadagno) {
		this.fattoreGuadagno = fattoreGuadagno;
	}
	
	@Override
	public String toString() {
		return "Prodotti [etichetta=" + etichetta + ", prezzoA=" + prezzoA + ", subcomponenti=" + subcomponenti + ", paeseProvenienza="
				+ paeseProvenienza + "]";
	}
}
