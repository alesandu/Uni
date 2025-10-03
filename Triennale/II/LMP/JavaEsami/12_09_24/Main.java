package esame1;
import java.time.LocalDate;
import java.util.ArrayList;

public class Main {
    public static void main(String[] args) {
        Prestito prestito1 = new Prestito(
            LocalDate.of(2024, 1, 10), 
            LocalDate.of(2024, 1, 24), 
            LocalDate.of(2024, 1, 30), 
            "Mario Rossi", 
            20.0
        );
        
        Prestito prestito2 = new Prestito(
            LocalDate.of(2024, 1, 25), 
            LocalDate.of(2024, 2, 8), 
            "Luigi Bianchi", 
            25.0
        );
        
        Prestito prestito3 = new Prestito(
            LocalDate.of(2024, 2, 5), 
            LocalDate.of(2024, 2, 19), 
            "Anna Verdi", 
            30.0
        );

        ArrayList<Prestito> prestitiLibro = new ArrayList<>();
        prestitiLibro.add(prestito1);
        prestitiLibro.add(prestito2);
        prestitiLibro.add(prestito3);
        
        Libro libro = new Libro(
            "La Tempesta", 
            "Feltrinelli", 
            2024, 
            prestitiLibro,
            320
        );

        ArrayList<Prestito> prestitiDVD = new ArrayList<>();
        prestitiDVD.add(prestito2);
        prestitiDVD.add(prestito3);

        DVD dvd = new DVD(
            "Inception", 
            "Warner Bros", 
            2010, 
            prestitiDVD, 
            148 
        );

        int periodoPiuLungoLibro = libro.calcolaPeriodoPiuLungoPrestito();
        System.out.println("Il periodo più lungo di prestito del libro è: " + periodoPiuLungoLibro + " giorni");

        ArrayList<String> incongruenzeLibro = libro.verificaIncongruenze();
        if (incongruenzeLibro.isEmpty()) {
            System.out.println("Nessuna incongruenza trovata per il libro.");
        } else {
            System.out.println("Incongruenze trovate per il libro:");
            for (String incongruenza : incongruenzeLibro) {
                System.out.println(incongruenza);
            }
        }

        int periodoPiuLungoDVD = dvd.calcolaPeriodoPiuLungoPrestito();
        System.out.println("Il periodo più lungo di prestito del DVD è: " + periodoPiuLungoDVD + " giorni");

        ArrayList<String> incongruenzeDVD = dvd.verificaIncongruenze();
        if (incongruenzeDVD.isEmpty()) {
            System.out.println("Nessuna incongruenza trovata per il DVD.");
        } else {
            System.out.println("Incongruenze trovate per il DVD:");
            for (String incongruenza : incongruenzeDVD) {
                System.out.println(incongruenza);
            }
        }
    }
}
