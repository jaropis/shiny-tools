### This file is part of PCSS's Run Test suite.

### Run Test is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.

### Run Test is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.

### You should have received a copy of the GNU General Public License
### along with Run Test.  If not, see <http://www.gnu.org/licenses/>.
### Run Test is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.

### Run Test is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.

### You should have received a copy of the GNU General Public License
### along with Run Test.  If not, see <http://www.gnu.org/licenses/>.

#' contains constantst used in building shiny apps, like list of colors, etc.
#'
#' @param accepts the language in which the app will be written
#' @return returns the color list in the appropriate language
#' @export
colorList <- function(language){
  if (language == "pl") return (list("pomarańczowy" = "orange",
     "biały"="white",
     "szary" = "gray",
     "czerwony"="#CC6666",
     "fioletowy"="#9999CC",
     "zielony"="#66CC99", 
     "czarny" = "black"))
  else return(list("orange" = "orange",
     "white"="white",
     "gray" = "gray",
     "red"="#CC6666",
     "violet"="#9999CC",
     "green"="#66CC99")
)
}

#' adds the description of the RunTest_linear_regression app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_linear_regression
#' @export
linear_regression_description <- function(language){
  if (language=="pl")
    return("Program do obliczania parametrów regresji liniowej. W tabelce poniżej znajduje się informacja o punkcie przecięcia z osią y (Intercept) oraz współczynnik nachylenia prostej. W drugiej kolumnie znajduje się wartość <em>p</em> a w trzeciej przedział ufności dla odpowiednich wielkości.<br>
    Dane można wprowadzać na trzy sposoby:
<ul>
   <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
   <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
   <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma zmienną niezależną a w drugiej zmienną zależną.
</ul>")
    else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_nonlinear_reg app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_nonlinear_reg
#' @export
nonlinear_regression_description <- function(language){
  if (language=="pl")
    return("Program do dopasowania danych empirycznych przy pomocy lokalnego dopasowania wielomianami. Wynik dopasowania zaprezentowany jest przy pomocy wykresu, a obliczone wartości znaleźć można w zakładce <em>Dane</em>.
    Dane można wprowadzać na trzy sposoby:
<ul>
   <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
   <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
   <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma zmienną niezależną a w drugiej zmienną zależną.
</ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_logistic_regression app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_logistic_regression
#' @export
logistic_regression_description <- function(language){
  if (language=="pl")
    return("Program do obliczania parametrów regresji logistycznej. W tabelce poniżej znajduje się informacja o ilorazie szans oraz stałej modelu (Intercept). W drugiej kolumnie znajduje się wartość <em>p</em>, a w trzeciej przedział ufności dla odpowiednich wielkości.<br>
           Dane można wprowadzać na trzy sposoby:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma zmienną niezależną, a w drugiej zmienną zależną.
           <li> <strong> Uwaga! Program nie zareaguje w przypadku gdy zmienna zależna <em>nie</em> będzie zmienną binarną.</strong>
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_SurvivalSuite_KM app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_SurvivalSuite_KM
#' @export
Survival_KM_description <- function(language){
  if (language=="pl")
    return("Program do tworzenia i porównywania krzywych Kaplana-Meiera prz pomocy testu log-rank. Porównywać można maksymalnie do czterech krzywych, które na rysunku mają kolory \"czerwony\", \"zielony\", \"niebieski\", \"magenta\", zgodnie z kolejnością alfabetyczną nazw grup. Pod wykresem krzywych znajduje się wartość zmiennej chi^2 oraz wartość <em>p</em>.<br>
            Do pierwszego pola należy wkleić czas w badaniu, do drugiego status przy opuszczeniu badania, a do trzeciego podział na grupy odpowiadający kolejnym porównywanym krzywym Kaplana-Meiera.
           Dane można wprowadzać na trzy sposoby:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma zmienną niezależną a w drugiej zmienną zależną.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_SurvivalSuite_Cox app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_SurvivalSuite_KM
#' @export
Cox_regression_description <- function(language){
  if (language=="pl")
    return("Program do budowania modelu regresji Coxa, prezentacji wyników przy pomocy wykresu typu <strong>forest</strong> oraz tabeli.<br>
           Do pierwszego pola należy wkleić skopiowaną z Excela tabelę zawierającą w kolejnych kolumnach: czas w badaniu, status przy opuszczeniu badania (1 - wystąpienie zdarzenia, 0 - niewystąpienie zdarzenia), a w kolejnych kolumnach pozostałe zmienne wchodzące do modelu. Można ograniczyć liczbę zmiennych wybierając procedurę selekcji wstecznej.<br>
  Innym sposobem wprowadzenia danych jest załadowanie arkusza kalkulacyjnego o opisanej wyżej strukturze do programu przy pomocy przycisku po lewej stronie.")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_log_contingency app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_log_contingency
#' @export
logistic_regression_2x2_description <- function(language){
  if (language=="pl")
    return("Program do obliczania parametrów regresji logistycznej dla tabeli kontyngencji 2x2. W tabelce poniżej znajduje się informacja o ilorazie szans oraz stałej modelu (Intercept). W drugiej kolumnie znajduje się wartość <em>p</em> a w trzeciej przedział ufności dla odpowiednich wielkości.<br>
           Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> w Excelu lub OpenOffice podświetlamy tabelkę, która ma zarówno nazwy kolumn jak i nazwy wierszy (oczywiście komórka A1 jest pusta), Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu zawiera tabelkę - w pierwszym wierszu znajdują się nazwy kolumn, w pierwszej kolumnie nazwy wierszy  (oczywiście komórka A1 jest pusta).
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_Marquardt app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_Marquardt
#' @export
Marquardt_description <- function(language){
  if (language=="pl")
    return("Program do dopasowywania funkcji jednej zmiennej do danych metodą uogólnionych najmniejszych kwadratów.<br>
            Funkcję należy wprowadzić do trzeciego okienka od góry, a wartości startowe stałych do czwartego. <strong> UWAGA! Jeżeli program nie reaguje to znaczy, że albo wzór został wpisany nieprawidłowo, albo nie wszystkie stałe mają wartości startowe, albo stałych jest zbyt wiele (pozstały po wcześniejszych próbach)</strong>.<br>
            Dane można wprowadzić:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v.
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma zmienną niezależną a w drugiej zmienną zależną.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_boot_confint app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_boot_confint
#' @export
boot_confint_description <- function(language){
  if (language=="pl")
    return("Program do znajdowania przedziałów ufności najprostszą metodą nieparametryczną dla statystyk zdefiniowanych na zbiorze danych dostarczonych przez użytkownika.<br>
           Funkcję należy wprowadzić do drugiego okienka od góry. Funkcja musi zwracać jedną liczbę - można używać takich funkcji jak <em>mean</em>, <em>median</em>, <em>sqrt</em> itd. <strong> UWAGA! Jeżeli program nie reaguje to znaczy, że wzór został wpisany nieprawidłowo. W takim przypadku program obliczy przedział ufności dla mediany.</strong>.<br>
           Dane można wprowadzić:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie zawiera analizowane dane.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_Pearson_correlation app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_Pearson_correlation
#' @export
Pearson_correlation_description <- function(language){
  if (language=="pl")
    return("Program do obliczania parametrów testu korelacji Pearsona pomiędzy dwiema zmiennymi. W tabelce poniżej znajduje się informacja o punkcie przecięcia z osią y (Intercept) oraz współczynnik nachylenia prostej. W drugiej kolumnie znajduje przedział ufności dla współczynnika korealcji, a w trzeciej wartość <em>p</em>.<br>
    Dane można wprowadzać na trzy sposoby:
<ul>
   <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
   <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
   <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu w pierwszych dwóch kolumnach zawiera korelowane zmienne.
</ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_Spearman_correlation app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_Spearman_correlation
#' @export
Spearman_correlation_description <- function(language){
  if (language=="pl")
    return("Program do obliczania parametrów testu korelacji Spearmana pomiędzy dwiema zmiennymi. W tabelce poniżej znajduje się informacja o punkcie przecięcia z osią y (Intercept) oraz współczynnik nachylenia prostej. W drugiej kolumnie znajduje się wartość <em>p</em>.<br>
    Dane można wprowadzać na trzy sposoby:
<ul>
   <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
   <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
   <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu w pierwszych dwóch kolumnach zawiera korelowane zmienne.
</ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_t_test app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_t_test
#' @export
ttest_description <- function(language){
  if (language=="pl")
    return("Program do porównywania dwóch grup przy pomocy testu t, opcjonalnie przy pomocy testu Wilcoxona.
            W tabelce znajdują się podstawowe informacje o porównywanych grupach, pod tabelką informacje o wynikach porównania, z przedziałem ufności dla różnicy średnich lub średniej różnicy, w zależności od wybranego testu.<br>
    Dane można wprowadzać na trzy sposoby:
<ul>
   <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
   <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
   <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu w pierwszych dwóch kolumnach zawiera porównywane zmienne.
</ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_Bland_Altman app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_Bland_Altman
#' @export
BA_description <- function(language){
  if (language=="pl")
    return("Program do tworzenia wykresu Blanda-Altmana i obliczania podstawowych jego charakterystyk.
           Dane można wprowadzać na trzy sposoby:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszych dwóch kolumnach zawiera analizowane pomiary.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_ANOVA_one_way app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_ANOVA_one_way
#' @export
ANOVA_description <- function(language){
  if (language=="pl")
    return("Program do porównywania grup przy pomocy analizy wariancji (ANOVA).<br>
Należy podać kolumnę danych z wartościami numerycznymi, oraz, w drugim polu, kolumnę z podziałem na grupy (do której grupy dana wartość należy)<br>.
            
    Dane można wprowadzać na trzy sposoby:
<ul>
   <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji
   <li> w Excelu lub OpenOffice podświetlamy kolumnę (kolumny nie powinny mieć nazw), Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v
   <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma wartości liczbowe a w drugiej podział na grupy. Kolumny nie powinny mieć nazw.
</ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_Kruskall_Wallis_test app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_Kruskall_Wallis_test
#' @export
Kruskall_Wallis_description <- function(language){
  if (language=="pl")
    return("Program do porównywania grup przy pomocy nieparametrycznej analizy wariancji (test Kruskalla-Wallisa).<br>
           Należy podać kolumnę danych z wartościami numerycznymi, oraz, w drugim polu, kolumnę z podziałem na grupy (do której grupy dana wartość należy).<br>
           
           Dane można wprowadzać na trzy sposoby:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę (kolumny nie powinny mieć nazw), Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma wartości liczbowe a w drugiej podział na grupy. Kolumny nie powinny mieć nazw.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_wilcoxon_test app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_wilcoxon_test
#' @export
wtest_description <- function(language){
  if (language=="pl")
    return("Program do porównywania dwóch grup przy pomocy testu Wilcoxona, opcjonalnie przy pomocy testu t.
            W tabelce znajdują się podstawowe informacje o porównywanych grupach, pod tabelką informacje o wynikach porównania, z opcjonalnym przedziałem ufności dla różnicy średnich lub średniej różnicy, w zależności od wybranego testu.<br>
    Dane można wprowadzać na trzy sposoby:
<ul>
   <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
   <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
   <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, w pierwszych dwóch kolumnach zawiera porównywane wielkości.
</ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_t_test_single app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_t_test_single
#' @export
ttest_single_description <- function(language){
  if (language=="pl")
    return("Program do testowania hipotezy o średniej grupy przy pomocy testu t, opcjonalnie przy pomocy testu Wilcoxona.
           W tabelce znajdują się podstawowe informacje grupie, pod tabelką informacje o wynikach porównania, z przedziałem ufności dla średniej.<br>
           Dane można wprowadzać na trzy sposoby:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie zawiera wartości pomiarów.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_AUC app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_AUC
#' @export
ROC_AUC_description <- function(language){
  if (language=="pl")
    return("Program tworzenia krzywej ROC (Receiver Operator Curve) oraz obliczania pola pod tą krzywą (AUC).<br>
           Dane można wprowadzać na trzy sposoby:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie zawiera wartości predyktora a w drugiej odpowidź.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_wilcoxon_test_single app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_wilcoxon_test
#' @export
wtest_single_description <- function(language){
  if (language=="pl")
    return("Program do testowania hipotezy o medianie przy pomocy testu Wilcoxona, opcjonalnie przy pomocy testu t.
           W tabelce znajdują się podstawowe grupie, pod tabelką informacje o wynikach porównania.<br>
           Dane można wprowadzać na trzy sposoby:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie zawiera wartości pomiarów.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_normality_test app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_normality_test
#' @export
normalitytest_description <- function(language){
  if (language=="pl")
    return("Program do sprawdzania normalności zmiennej.<br>
    Dane można wprowadzać na trzy sposoby:
<ul>
   <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
   <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
   <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie zawiera badaną zmienną.
</ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_normality_test app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_normality_test
#' @export
BoxCox_description <- function(language){
  if (language=="pl")
    return("Program do obliczania parametru lambda w metodzie Boxa-Coxa.<br>
           Dane można wprowadzać na trzy sposoby:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie zawiera transformowaną zmienną.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}
#' adds the description of the RunTest_onesummary app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_normality_test
#' @export
onesummary_description <- function(language){
  if (language=="pl")
    return("Podsumowanie jednej zmiennej.<br>
           Dane można wprowadzać na trzy sposoby:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie zawiera analizowaną zmienną.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_summary_discrete_single app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_summary_discrete_single
#' @export
single_categorical_description <- function(language){
  if (language=="pl")
    return("Program do opisu pojedynczej zmiennej kategorycznej.<br>
           Dane można wprowadzać na trzy sposoby:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie zawiera analizowaną zmienną.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}
#' adds the description of the RunTest_polynomial_regression app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_polynomial_regression
#' @export
polynomial_regression_description <- function(language){
  if (language=="pl")
    return("Program do dopasowania modelu regresji wielomianowej rzędu maksymalnie 15 lub rzędu wynikającego z danych. W tabelce poniżej znajdują się informacje o wartościach dopasowanych stałych przed elementami odpowiednich rzędów - od 0 (stała) do <em>n</em> wraz z 95% przedziałem ufności oraz wartością <em>p</em> dla dopasowania.<br>
           Dane można wprowadzać na trzy sposoby:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma zmienną niezależną a w drugiej zmienną zależną.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_multiple_regression app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_multiple_regression
#' @export
multiple_regression_description <- function(language){
  if (language=="pl")
    return("Program do dopasowania modelu regresji wielorakiej. W tabelce poniżej znajdują się informacje o parametrach modelu wraz z 95% przedziałem ufności oraz wartością <em>p</em> dla dopasowania. Poniżej znajdują się informacje o dopasowanym modelu.<br>
           Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> w Excelu lub OpenOffice podświetlamy blok zawierający dane, pierwsza kolumna musi zawierać zmienną zależną. Kolumny mogą mieć nazwy, które zostaną użyte w obliczeniach. Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma zmienną zależną, a w pozostałych zmienne niezależne. Kolumny mogą mieć nazwy, które zostaną użyte w prezentowanych wynikach.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_correlation_matrix_Pearson
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_correlation_matrix_Pearson
#' @export
cor_matrix_p_description <- function(language){
  if (language=="pl")
    return("Program do obliczania i wizualizacji macierzy korelacji.<br>
           Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> w Excelu lub OpenOffice podświetlamy blok zawierający dane. Kolumny muszą mieć nazwy, które zostaną użyte w obliczeniach. Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu zawiera dane reprezentowane w sposób opisany w poprzednim pukncie.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_correlation_matrix_Spearman
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_correlation_matrix_Spearman
#' @export
cor_matrix_s_description <- function(language){
  if (language=="pl")
    return("Program do obliczania i wizualizacji macierzy korelacji.<br>
           Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> w Excelu lub OpenOffice podświetlamy blok zawierający dane, pierwsza kolumna musi zawierać zmienną zależną. Kolumny mogą mieć nazwy, które zostaną użyte w obliczeniach. Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma zmienną zależną, a w pozostałych zmienne niezależne. Kolumny mogą mieć nazwy, które zostaną użyte w obliczeniach.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_multiple_logistic_regression app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_multiple_logistic_regression
#' @export
multiple_logistic_regression_description <- function(language){
  if (language=="pl")
    return("Program do dopasowania modelu logisticznej regresji wielorakiej. W tabelce poniżej znajdują się informacje o parametrach modelu wraz z 95% przedziałem ufności oraz wartością <em>p</em> dla dopasowania. Poniżej znajdują się informacje o dopasowanym modelu.<br>
           Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> w Excelu lub OpenOffice podświetlamy blok zawierający dane, pierwsza kolumna musi zawierać zmienną zależną. Kolumny mogą mieć nazwy, które zostaną użyte w prezentacji wyników. Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma zmienną zależną, a w pozostałych zmienne niezależne. Kolumny muszą mieć nazwy, które zostaną użyte w obliczeniach.
           <li> <strong> Uwaga! Program nie zareaguje w przypadku gdy zmienna zależna <em>nie</em> będzie zmienną binarną.</strong>
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_three_way app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_three_way
#' @export
three_way_description <- function(language){
  if (language=="pl")
    return("Program do analizy tabeli kontyngencji 2x2xk przy pomocy metody Cochrana-Mantela-Haenszela oraz analizy jednorodności związków przy pomocy testu Breslowa-Daya. Wynikiem programu jest tabela podsumowująca ilorazy szans na poszczególnych poziomach trzeciej zmiennej, iloraz szans dla pełnej tabeli oraz wynik testu Breslowa-Daya.<br>
           Dane wejścowe to tabela, która w pierwszej kolumnie ma zmienną zależną, w drugiej niezależną, a w trzeciej zmienną, na której poziomach obliczane będą podtabele. Program nie zadziała o ile pierwsze dwie kolumny nie będą binarne. Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> w Excelu lub OpenOffice podświetlamy blok zawierający dane. Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu zawiera tabelę.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_multiple_logistic_regression app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_multiple_logistic_regression
#' @export
agreement_description <- function(language){
  if (language=="pl")
    return("Program do obliczania parametru kappa zgodności pomiędzy oceniającymi dla dwóch (metoda Cohena) i więcej (metoda Fleissa) oceniających.
           Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> w Excelu lub OpenOffice podświetlamy blok zawierający dane. Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela zawierający dane.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_Poisson_regression app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_multiple_logistic_regression
#' @export
Poisson_regression_description <- function(language){
  if (language=="pl")
    return("Program do dopasowania modelu regresji Poissonowskiej. W tabelce poniżej znajdują się informacje o wartościach dopasowanych stałych przed elementami odpowiednich rzędów - od 0 (stała) do <em>n</em> wraz z 95% przedziałem ufności oraz wartością <em>p</em> dla dopasowania.<br>
           Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> w Excelu lub OpenOffice podświetlamy blok zawierający dane, pierwsza kolumna musi zawierać zmienną zależną. Kolumny mogą mieć nazwy, które zostaną użyte w obliczeniach. Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma zmienną zależną, a w pozostałych zmienne niezależne. Kolumny mogą mieć nazwy, które zostaną użyte w obliczeniach.
           <li> <strong> Uwaga! Program nie zareaguje w przypadku gdy zmienna zależna <em>nie</em> będzie zmienną binarną.</strong>
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_ordinal_logistic_regression app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_ordinal_logistic_regression
#' @export
ordered_logistic_regression_description <- function(language){
  if (language=="pl")
    return("Program do dopasowania modelu uporządkowanej regresji logistycznej. W tabelce poniżej znajdują się informacje o wartościach dopasowanych parametrów wraz z 95% przedziałem ufności oraz wartością <em>p</em> dla dopasowania. Pod tabelą znajduje się podsumowanie modelu.<br>
            Program wykorzystuje założenie proporcjonalnych szans (proportional odds asumption)<br>
            Aby użyć programu musimy wprowadzić zmienną niezależą (x), zależną (y) oraz podać <strong>porządek poziomów zmiennej zależnej</strong><br>
           Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> wprowadzając dane z klawiatury,
           <li> w Excelu lub OpenOffice podświetlamy blok zawierający dane, pierwsza kolumna musi zawierać zmienną zależną, a druga niezależną. Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma zmienną zależną, a w drugiej niezależną. Kolumny nie mogą mieć nazw. <strong>W drugim skoroszycie w pierwszej kolumnie należy <em>rosnąco</em> wprowadzić poziomy.</strong>
           <li> <strong> Uwaga! Program nie zareaguje w przypadku gdy poziomy zmiennej zależnej <em>nie</em> będzą odpowiadały poziomom wprowadzonym w okienku z kolejnością poziomów.</strong>
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_prop_many app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_prop_many
#' @export

multiple_prop_test_description <- function(language){
  if (language=="pl")
    return("Program do porównywania proporcji w grupach. W tabelce poniżej znajduje się informacja o proporcjach wyliczonych z danych wejściowych, a poniżej wynik testu (wartość <em>p</em>).<br>
           Dane wprowadzamy wpisując w pierwsze pole liczbę zdarzeń (obserwacji) sprzyjających, a w drugie pełną liczebność grup, w obu przypadkach oddzielając liczby spacjami.")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_prop_two app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_prop_two
#' @export

double_prop_test_description <- function(language){
  if (language=="pl")
    return("Program do porównywania proporcji w dwóch grupach. W tabelce poniżej znajduje się informacja o proporcjach wyliczonych z danych wejściowych, a poniżej wynik testu (wartość <em>p</em>) oraz przedział ufności dla <strong>różnicy</strong> pomiędzy dwiema proporcjami.<br>
           Dane wprowadzamy wpisując w pierwsze pole liczbę zdarzeń (obserwacji) sprzyjających, a w drugie pełną liczebność grup, w obu przypadkach oddzielając liczby spacjami.")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_prop_one app
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_prop_one
#' @export

single_prop_test_description <- function(language){
  if (language=="pl")
    return("Program do porównywania proporcji wynikającej z danych do z góry założonej proporcji teoretycznej. W tabelce poniżej znajduje się informacja o proporcji wyliczonyej z danych wejściowych, a poniżej wynik testu (wartość <em>p</em>) dla testu równości proporcji z danych i wartości teoretycznej oraz oraz przedział ufności dla proporcji.<br>
           Dane wprowadzamy wpisując w pierwsze pole liczbę zdarzeń (obserwacji) sprzyjających, w drugie pełną liczebność, w obu przypadkach oddzielając liczby spacjami, a w trzece proporcję teoretyczną.")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_chi_squared
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_chi_squared
#' @export

chi_squared_test_description <- function(language){
  if (language=="pl")
    return("Program do testowania losowości tabeli kontyngencji.<br>
           Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> w Excelu lub OpenOffice podświetlamy blok zawierający dane - kolumny i wiersze nie mogą mieć nazw, wklejamy same liczby z tabeli. Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu zawiera tabelę, bez nazw wierszy i kolumn - same liczby
           <li> jeżeli tabela zawiera wartości mniejsze niż 5, wyniki mogą być nieprawidłowe - użytkownik zostanie o tym poinformowany
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
  }

#' adds the description of the RunTest_Fisher
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_Fisher
#' @export

Fisher_test_description <- function(language){
  if (language=="pl")
    return("Program do testowania losowości miacierzy, a w przypadku macierzy 2 x 2 również do obliczania ilorazu szans.<br>
           Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> w Excelu lub OpenOffice podświetlamy blok zawierający dane - kolumny i wiersze nie mogą mieć nazw, wklejamy same liczby z tabeli. Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu zawiera tabelę, bez nazw wierszy i kolumn - same liczby.
           <li> dla tabel wymiaru 2 x 2 program podaje iloraz szans i 95% przedział ufności dla tego ilorazu
           <li> Zbyt duża tabela może spowodować wyczerpanie pamięci. W takim przypadku należy użyć testu chi^2 lub uruchomić obliczenia na infrasrukturze PCSS.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
  
}

#' adds the description of the RunTest_Fisher
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_Fisher
#' @export

odds_ratio_test_description <- function(language){
  if (language=="pl")
    return("Program do obliczania ilorazu szans dla macierzy 2 x 2 oraz testowaniu hipotezy, że iloraz ten jest równy 1.<br>
           Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> w Excelu lub OpenOffice podświetlamy blok zawierający dane - kolumny i wiersze nie mogą mieć nazw, wklejamy same liczby z tabeli. Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu zawiera tabelę, bez nazw wierszy i kolumn - same liczby
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
  
}
#' adds the description of the RunTest_sensit
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_sensit
#' @export

sensit_description <- function(language){
  if (language=="pl")
    return("Program do obliczania czułości, swoistości, PPV, NPV, FPR i FDR dla macierzy 2 x 2.<br>
            Obiektem wejściowym jest tabela klasyfikacji wyników - przykładową konstrukcję zobaczyć można w zakładce \"Dane\" przed wprowadzeniem danych.<br>
           Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> w Excelu lub OpenOffice podświetlamy blok zawierający dane - kolumny i wiersze nie mogą mieć nazw, wklejamy same liczby z tabeli. Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu zawiera tabelę, bez nazw wierszy i kolumn - same liczby.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
  
}

#' adds the description of the RunTest_binom_trend
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_binom_trend
#' @export
binom_trend_description <- function(language){
  if (language=="pl")
    return("Program do sprawdzania obecności trendu w tabeli 2xk.
           Dane można wprowadzać na trzy sposoby:
           <ul>
           <li> wpisując dane bezpośrednio do okienek po lewej stronie aplikacji,
           <li> w Excelu lub OpenOffice podświetlamy kolumnę, Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszym wierszu ma pierwszy wiersz tabeli, a w drugim wierszu drugi wiersz tabeli. Wiersze i kolumny nie mogą mieć nazw.
           </ul>")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' adds the description of the RunTest_summary_categorical_multi
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the program RunTest_multiple_logistic_regression
#' @export
summary_categorical_multi_description <- function(language){
  if (language=="pl")
    return("Program do tworzenia tabeli płaskiej (flat table) dla danych kategorycznych w przypadku wielu zmiennych. 
           Dane można wprowadzać na dwa sposoby:
           <ul>
           <li> w Excelu lub OpenOffice podświetlamy blok zawierający dane, pierwsza kolumna musi zawierać zmienną zależną. Kolumny mogą mieć nazwy, które zostaną użyte w obliczeniach. Ctrl+c i po kliknięciu okienka po lewej stronie, Ctrl+v,
           <li> wybierając przy pomocy przycisku po lewej stronie plik Excela, który w pierwszym arkuszu, pierwszej kolumnie ma zmienną zależną, a w pozostałych zmienne niezależne. Kolumny mogą mieć nazwy, które zostaną użyte w wynikach.
           </ul>
           Patrz przykładowe dane w zakładce \"Dane\".")
  else  return("Software for calculating the parameters of linear regression. In the table below there is info on the intercept and the tangent of the line.")
}

#' the description of the "dependent variable" field
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the "dependent variable" field in the selected language
#' @export
dependentVariable <- function(language){
  if (language=="pl") return("zmienna zależna - wpisz oddzielając liczby spacjami lub skopiuj i wklej kolumnę z Excela")
  else return("dependent variable - enter the numbers with spaces between them or copy and paste an Excel column")
}

#' the description of the "independent variable" field
#'
#' @param accepts the language in which the app will be written
#' @return the description of the "independent variable" field in the selected language
#' @export
independentVariable <- function(language){
  if (language=="pl") return("zmienna niezależna - wpisz oddzielając liczby spacjami lub skopiuj i wklej kolumnę z Excela")
  else return("independent variable - enter the numbers with spaces between them or copy and paste an Excel column")
}

#' the description of the "variable no i" field
#'
#' @param accepts the language in which the app will be written and the number of the variable
#' @return the description of the "variable no i" field in the selected language
#' @export
variableI <- function(language,i){
  if (language=="pl") return(paste("zmienna  ",i," - wpisz oddzielając liczby spacjami lub skopiuj i wklej kolumnę z Excela"))
  else return(paste("variable ",i," - enter the numbers with spaces between them or copy and paste an Excel column"))
}

#' the description of the "x name" field
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the "x name" field in the selected language
#' @export
xDescription <- function(language){
  if (language=="pl") return("opis zmiennej x")
  else return("x variable description")
}

#' the description of the "y name" field
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the "y name" field in the selected language
#' @export
yDescription <- function(language){
  if (language=="pl") return("opis zmiennej y")
  else return("y variable description")
}

#' the description of the "variable name" field
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the "variable name" field in the selected language
#' @export
variableName <- function(language){
  if (language=="pl") return("nazwa zmiennej")
  else return("y variable name")
}
#' the description of the "file upload" field
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the "file upload name ad type" field in the selected language
#' @export
loadFile <- function(language, typ){
  if (language=="pl") return(paste("załaduj plik",typ))
  else return(paste("upload",typ,"file"))
}

#' the description of the "color select" field
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the "color select" field in the selected language
#' @export
kolor <- function(language){
  if (language=="pl") return("kolor")
  else return("color")
}


#' the description of the "theoretical quantiles" axis
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the "theoretical quantiles" axis in the selected language
#' @export
theoreticalQuantilesDescription <- function(language){
  if (language=="pl") return("kwantyle teoretyczne")
  else return("theoretical quantiles")
}

#' the description of the "sample quantiles" axis
#'
#' @param accepts the language in which the app will be written
#' @return the information line for the "sample quantiles" axis in the selected language
#' @export
sampleQuantilesDescription <- function(language){
  if (language=="pl") return("kwantyle z próby")
  else return("sample quantiles")
}

#' the description of the "multiple regression" backward selection field
#'
#' @param accepts the logical value for outputing "backward selection" and the language in which the app will be written
#' @return the "backward selection" in the appropriate language or an empty string
#' @export
backwardSelection <- function(step, language){
  if (step)
    if (language=="pl") return("z selekcją wsteczną")
  else return("with backward selection")
  else return("")
}

#' the description of the "multiple data entry" field
#'
#' @param accepts the language in which is written
#' @return the description of the "independent variable" field in the selected language
#' @export
describeMultiple <- function(language){
  if (language=="pl") return("dane - skopiuj i wklej kolumny z Excela - patrz wyjaśnienia")
  else return("dependent variable - enter the numbers with spaces between them or copy and paste an Excel column")
}

#' the description of the "approx formula" field in the Marquardt - Levenberg application
#'
#' @param accepts the language in which is written
#' @return the description of the "approx formula" field in the selected language
#' @export
approxFormula <- function(language){
  if (language=="pl") return("wpisz wzór funkcji dopasowywanej do danych - użyj C1, C2 ... jako nazwy stałych, które chcesz znaleźć")
  else return("dependent variable - enter the numbers with spaces between them or copy and paste an Excel column")
}
