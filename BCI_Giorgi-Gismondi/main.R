library("LiblineaR")

#Lettura dei vari file .txt e creazione di un unico dataset, affiancando
#il dataset delle X, il dataset delle C ed il dataset delle Y
c_dataset <- read.csv("C.txt", header = F, stringsAsFactors = F)
y_dataset <- read.csv("Y.txt", header = F, stringsAsFactors = F)
names(y_dataset) <- c("label")
x_dataset <- read.delim("X.txt", header = F, sep = " ", dec = ".")
dataset <- cbind(x_dataset, c_dataset, y_dataset)

#Effettuiamo un plot per confrontare la media di tutti i segnali target e di
#tutti i non target per ciascun canale (quindi 8 grafici in totale)
source("data_understanding.R")
a <- data_understanding(dataset)

#Vediamo che per i canali 4 e 7 la media dei segnali target è molto rumorosa.
#Pertanto, eliminiamo dal dataset questi due canali.
source("delete_channels.R")
red_data <- delete_channels(dataset, 4, 7)

#Per ciascun canale effettuiamo una operazione di filtraggio passa-basso:
#smorziamo le variazioni veloci del segnale, ossia probabili errori di misura.
source("low_pass_filter.R")
red_data <- low_pass_filter(red_data)

#Per ciascun canale effettuiamo una operazione di down-sampling:
#prendendo meno campioni si mantiene l'andamento generale della
#stimolazione e si perdono altri probabili rumori.
source("reduce_sampling_frequency.R")
red_data <- reduce_sampling_frequency(red_data)

#Per comodità, ordiniamo il dataset a blocchi di 120 righe (12 * numero di
#iterazioni per carattere) rispetto alla colonna delle C.
source("order_dataset.R")
ordered_dataset <- order_dataset(red_data)

#Suddividiamo il dataset in tanti subdataset di 120 righe, ossia ogni
#subdataset è relativo ad un solo carattere.
source("split_by_char.R")
char_split_dataset <- split_by_char(ordered_dataset)

#Consideriamo un "nuovo" dataset in cui, per ogni carattere, effettuiamo la
#media delle 10 misurazioni fatte in corrispondenza dell'illuminazione di una
#determinata riga/colonna relativa al carattere stesso. Otteniamo, dunque, un
#dataset in cui le 120 righe relative alle 10 iterazioni delle osservazioni di
#un certo carattere si riducono a 12 righe.
source("calculate_mean_matrix.R")
mean_matrix <- calculate_mean_matrix(char_split_dataset)

#Calcoliamo il segnale di riferimento per i segnali target, ottenuto prendendo
#la media di tutte le osservazioni relative a stimolazioni target.
source("find_reference_signal.R")
reference_signal <- find_reference_signal(red_data)
#Calcoliamo, per ogni osservazione del dataset "medio" (mean_matrix), la
#differenza in modulo di ciascun campione con i corrispondenti campioni del
#segnale di riferimento: i segnali non target tenderanno ad essere più
#"distanti" dal segnale di riferimento.
source("add_reference_difference.R")
ext_data <- add_reference_difference(mean_matrix, reference_signal)

#In fase di test abbiamo notato che alcuni segnali target venivano mal
#classificati poiché possedevano un picco "convesso" piuttosto che un picco
#"concavo" (che è quello che il segnale di riferimento possiede). Questo
#comporta una differenza tra il segnale target in questione ed il segnale
#di riferimento maggiore di quella di un segnale non target (senza alcun picco)
#ed il segnale di riferimento. Al fine di compensare questo inconveniente,
#introduciamo un parametro indipendente dalla convessità del picco, ovvero
#l'energia del segnale.
source("add_energy.R")
final_dataset <- add_energy(ext_data)

#Consideriamo come training set 5 parole e come test set la rimanente parola
source("split_dataset.R")
test_word = 1 #permette di selezionare la parola di test
splitted_data <- split_dataset(final_dataset, test_word)

#Normalizzazione del dataset
source("data_normalization.R")
normalized_splitted_data <- data_normalization(splitted_data)
source("get_scale_values.R")
scale_values <- get_scale_values(final_dataset) #prendo il valore di scalatura del training set
#che poi sarà riutilizzato nella funzione "normalization" più avanti nel suo test

#Analizzando vari paper sull'argomento, abbiamo visto che i risultati migliori
#sono forniti dalla SVM lineare. In alcuni casi viene utilizzata anche la SVM
#Gaussiana ma conduce a risultati simili della lineare. Sempre dai vari paper,
#abbiamo visto che i valori del costo 'c' utilizzati sono tipicamente compresi
#tra 0.01 e 1. Nel nostro caso abbiamo provato valori da 0.001 fino a 0.1 ma
#i risultati ottenuti sono analoghi per tutti i casi e tendono a peggiorare per
#le c > 0.1. Scegliamo, dunque, c = 0.01 che fornisce i risultati migliori
source("setting_linear_svm.R")
c_setting <- setting_linear_SVM(normalized_splitted_data)
View(c_setting)

#Addestriamo la SVM e classifichiamo il test set
source("test_svm.R")
test_results <- test_svm(
  normalized_splitted_data$scaled_training,
  normalized_splitted_data$label_train,
  normalized_splitted_data$scaled_test,
  normalized_splitted_data$label_test
)
test_results
#Se usiamo la prima parola come test set, vengono classificati correttamente
#tutti i punti target, ossia tutti i caratteri sono stati classifcati
#correttamente

for (i in 2:6) {
  test_word = i
  splitted_data <- split_dataset(final_dataset, test_word)
  normalized_splitted_data <- data_normalization(splitted_data)
  test_results <- test_svm(
    normalized_splitted_data$scaled_training,
    normalized_splitted_data$label_train,
    normalized_splitted_data$scaled_test,
    normalized_splitted_data$label_test
  )
  print(test_results$correct_target_points)
}
#se usiamo la seconda parola come test set otteniamo nuovamente tutti i
#caratteri classificati correttamente; idem per la terza e la quarta.
#Per la quinta e la sesta parola, invece, otteniamo che il 90% di punti
#target sono classificati correttamente. Questo implica che per un carattere
#è stata indovinata la riga/colonna ma non la colonna/riga, ossia 4 caratteri
#classificati correttamente su 5.
#In percentuale, 93.33% di accuratezza su 5 caratteri

#Addestriamo l'SVM su tutto il dataset
source("svm.R")
complete_dataset <- rbind(normalized_splitted_data$scaled_training,
                          normalized_splitted_data$scaled_test)
complete_labels <- rbind(t(t(normalized_splitted_data$label_train)),
                         t(t(normalized_splitted_data$label_test)))
res <- svm(complete_dataset, complete_labels)

#(w,b) della SVM
w <- res$W[1:(length(res$W) - 1)]
b <- res$W[length(res$W)]

## INIZIO TEST SU DATASET DIVERSI DA QUELLI IN NOSTRA DOTAZIONE

#inserire qui i file di test
c_test <- read.csv("", header = F, stringsAsFactors = F)
y_test <- read.csv("", header = F, stringsAsFactors = F)
x_test <- read.delim("", header = F, sep = " ", dec = ".")
test_set <- cbind(x_test, c_test, y_test)

source("delete_channels.R")
red_data <- delete_channels(test_set, 4, 7)
source("low_pass_filter.R")
red_data <- low_pass_filter(red_data)
source("reduce_sampling_frequency.R")
red_data <- reduce_sampling_frequency(red_data)
source("order_dataset.R")
ordered_dataset <- order_dataset(red_data)
source("split_by_char.R")
char_split_dataset <- split_by_char(ordered_dataset)
source("calculate_mean_matrix.R")
mean_matrix <- calculate_mean_matrix(char_split_dataset)
source("add_reference_difference.R")
ext_data <- add_reference_difference(mean_matrix, reference_signal)
source("add_energy.R")
final_dataset <- add_energy(ext_data)
source("normalization.R")
normalized_test_set <- normalization(final_dataset, scale_values)

source("svm_classifications.R")
res <- svm_classifications(
          w,
          b,
          normalized_test_set$scaled_test,
          normalized_test_set$label_test)
#queste operazioni ri-mappano il vettore delle classificazioni (che nel nostro
#caso è stato ordinato rispetto alle C e ridotto in numero di righe poiché si è
#considerata la media di tutte le osservazioni) ad un vettore analogo e
#confrontabile con il file delle Y
source("extend_classifications.R")
ordered_classification <- extend_classifications(res$classification_vec)
source("adapt_classifications.R")
adapted_classification <- adapt_classifications(ordered_classification, c_test)
#le trasformazioni vengono "annullate" 

source("count_correct_characters.R")
correct_characters <- count_correct_characters(adapted_classification, y_test)
correct_characters / 5 #accuratezza (caratteri corretti su 5)

#########
#Poiché gli effetti più evidenti di una stimolazione avvengono a distanza di
#300ms, la nuova finestra temporale consiste nello scartare i campioni prima di
#tale istante di tempo: considerando che le misurazioni sono state compiute a
#256 Hz, allora l'intenzione era di toglierne 256 * 0.3 = 77 (circa). Tuttavia,
#ne abbiamo tolti solamente 64, "tagliando" solamente i primi 250 ms di
#misurazioni.

#Dalla primissima lettura del file delle "Y" fino alla classificazione,
#avvengono diverse trasformazioni, tra cui l'ordinamento rispetto alla colonna
#delle "C". Dopo aver eseguito extend_classifications e
#adapt_classifications si ottiene un vettore in cui le trasformazioni vengono
#"annullate". In questo modo è possibile verificare che in maniera diretta che
#il vettore delle classificazioni che la SVM fornisce è uguale al vettore che
#si legge direttamente dal file delle "Y". Abbiamo aggiunto questa operazione
#solamente qualora volesse utilizzare altre sue funzioni diverse dalle nostre per
#valutare la bontà dei risultati.