# İstihdamda kadın erkek eşitsizliği
Veri Görselleştirme Dersi Dönem Sonu Projesi

Türkiye iş gücü piyasasına bakıldığında, toplumsal cinsiyet eşitsizliklerinin yoğun olarak gözlemlendiği bir alan olan kadınların işgücüne ve istihdama katılım düzeylerinin
düşüklüğü hemen göze çarpıyor. Günümüzde her üç kadından yalnızca biri işgücünde. Erkeklerin istihdam oranı %65 civarındayken, kadınların istihdam oranı yalnızca %30.

```
#Colorcodes: darkgoldenrod1 for male, #9999FF for female

#KADINLAR VE ERKEKLERİN İSTİHDAM ORANLARININ KARŞILAŞTIRILMASI

#kadınların popülasyona ve erkeklerin populasyona oranlarını filtreledim.
ratio_fpmp <- data |>
  filter(`Indicator Code` %in% c("SL.EMP.TOTL.SP.FE.ZS", "SL.EMP.TOTL.SP.MA.ZS"))

#İkisinin filtrelenmiş halini görselleştirdim.
ggplot(ratio_fpmp, aes(x = as.numeric(Year), y = as.numeric(Value), color = `Indicator Code`)) +
  geom_line(size = 1.5) +  # Adjust line thickness here
  theme_minimal() +
  labs(x = "Yıl",
       y = "%",
       title = "Yıllar İçinde Kadın ve Erkek İstihdamının Karşılaştırılması",
       color = "Cinsiyet") +
  scale_color_manual(values = c("SL.EMP.TOTL.SP.FE.ZS" = "#9999FF", "SL.EMP.TOTL.SP.MA.ZS" = "darkgoldenrod1"),
                     labels = c("SL.EMP.TOTL.SP.FE.ZS" = "Kadın", "SL.EMP.TOTL.SP.MA.ZS" = "Erkek")) +
  guides(color = guide_legend(reverse = TRUE))+
  scale_y_continuous(limits = c(0, 100))

  ```

![1-Kadin_Erkek_istihdam](https://github.com/MaykothePsycho/istihdamda_kadin_erkek_esitsizligi/assets/121159197/27ca8dc7-1fe3-46f5-83be-889854ca6140)


# Ücretsiz ve Ev içi Emek

Hane içi eşitsiz iş yükü, kadın istihdamı önündeki engellerin ve işgücü piyasasındaki eşitsizliklerin temelinde alıyor. Kadınların içgücüne katılımının yıllar içinde artması, erkeklerin hane içi sorumlulukları daha fazla almasıyla değil, kadınların çok daha fazla sorumluluk üstlenip yerine getirmesiyle sonuçlanıyor.

Zaman kullanımı verileri, hane içi iş yükü konusunda kadın-erkek arasındaki eşitsizliğin tüm dünyada gözlendiğini gösteriyor. Türkiye’de ise hane içi emeğin göze çarpar düzeyde orantısız
biçimde kadınlar tarafından üstlenildiğini görüyoruz. Kadınlar bir günde oralama 4 saat 30 dakikayı hanehalkı ve ev bakımına harcarken, erkekler buna yalnızca 50 dakika harcıyor.

```
# Verisetini kullanabileceğim şekilde yeniden şekillendirdim. Bu görselde farklı bir dataset kullandım.
df_long <- melt(goal05_gender_time_use, id.vars = c("iso3c", "country", "date", "gap"), 
                measure.vars = c("male", "female"), 
                variable.name = "gender", 
                value.name = "percentage")

# NA değerleri çıkardım
df_long <- na.omit(df_long)

# Görselleştirme yaparken kadınlara göre sıralı bir hale getirmek için böyle bir fonksiyon kullandım.
df_long$country <- factor(df_long$country, levels = df_long$country[df_long$gender == "female"][order(df_long$percentage[df_long$gender == "female"])])

# Kısaltma kodları sütunu oluşturdum.
df_long$country_label <- df_long$iso3c

# Kullanacağım ülke kodlarını belirledim.
countries_to_label <- c("TUR", "USA", "DEU", "IND", "BRA", "ESP", "POL", "JPN", "CAN", "FRA", "ITA", "LAO", "QAT", "PER", "PRT", "CHE", "ETH", "MDA", "ALB", "EGY")

# Görselleştirdim
ggplot(data = df_long, aes(x = country, y = percentage, color = gender)) +
  geom_point(size = 3) +
  geom_line(aes(group = country), color = "grey") +
  scale_color_manual(values = c("male" = "darkgoldenrod1", "female" = "#9999FF"), 
                     labels = c("male" = "Erkek", "female" = "Kadın")) +
  labs(title = "Ülkelere Göre Kadın ve Erkeklerin Ücretsiz ve Ev içi Emeklere Harcadığı Saatler",
       x = "Ülke",
       y = "Saat",
       color = "Cinsiyet") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), # Remove country names from x-axis
        axis.ticks.x = element_blank()) +
  geom_text(data = subset(df_long, gender == "female" & iso3c %in% countries_to_label),
            aes(label =  iso3c, 
                y = percentage + 0.4),
            angle = 0, hjust = 0.5, size = 3)

```

![2-Ev_ici_ucretsiz](https://github.com/MaykothePsycho/istihdamda_kadin_erkek_esitsizligi/assets/121159197/e57167ca-5be6-4e4d-91c7-3332534234cc)


# Sektörlere göre kadın istihdamı

Dünyanın pek çok ülkesinde olduğu gibi Türkiye’de de kadın istihdamında en büyük
paya sahip sektör %55 ile hizmet sektörü. Tarımın kadın istihdamı içindeki payı %29
düzeyinde. Türkiye’de imalat sanati ise kadın istihdamında %15'le en düşük paya sahip.

```
# İlgili göstergelerin 2022 yılı değerlerini filtreledim. 
filtered_employ_2022 <- data_cleaned %>%
  filter(`Indicator Code` %in% c("SL.AGR.EMPL.FE.ZS", "SL.IND.EMPL.FE.ZS", "SL.SRV.EMPL.FE.ZS") & Year == 2022)

# Waffle chart için vector oluşturdum.
waffle_data <- setNames(filtered_employ_2022$Value, filtered_employ_2022$`Indicator Code`)

# Gösterge isimlerini oluşturdum.
names(waffle_data) <- recode(names(waffle_data),
                             "SL.AGR.EMPL.FE.ZS" = "Tarım",
                             "SL.IND.EMPL.FE.ZS" = "Sanayi",
                             "SL.SRV.EMPL.FE.ZS" = "Hizmet")

# Görselleştirdim
waffle(
  waffle_data,
  rows = 7,  # Number of rows in the waffle chart
  title = "Kadınların farklı sektörlere göre istihdam dağılımı"
)
```
![3-Sektorler](https://github.com/MaykothePsycho/istihdamda_kadin_erkek_esitsizligi/assets/121159197/135a53fb-dda3-417e-9227-6b2e2eb85af7)

# Kırılgan İstihdam

Kadınların hâlâ %26’sı ücretsiz aile işçisi konumunda çalışıyor. Ücretsiz aile işçiliği,
kadınların hane işletmesinde herhangi bir maddi karşılığı olmadan çalışmasını anlatıyor. Erkekler içinde ücretsiz aile işçisi olarak çalışanların oranı sadece %5.
Uluslararası Çalışma Örgütü (ILO) ücretsiz aile işçiliğini kendi hesabına çalışma ile birlikte ele alarak bu iki çalışma biçimini kırılgan istihdam olarak tanımlıyor. Kırılgan istihdam tanımından da anlaşıldığı üzere enformel / kayıtdışı ve düzensiz çalışma, düzensiz ücret ödemelerinin olduğu, iş garantisinin olmadığı, geçici, güvencesiz çalışma biçimleri anlamına geliyor. Bu özellikler genellikle kadın istihdamının yoğunlaştığı sektör ve mesleklerde gözleniyor. İşgücü piyasalarının esnekleşmesi ile ortaya çıkan yarı-zamanlı, geçici ve güvencesiz genel anlamda atipik işlerde çalışanlar da çoğunlukla kadınlar.

```
#PEKİ NASIL BİR İSTİHDAM?

#Kadınlar, güvencesiz ve düşük gelirli "eğreti" işlerde erkeklere göre çok daha fazla oranda çalışıyor.

#Data temizliği için ilk satırdaki açıklama bölümünü çıkardım, boş değerleri temizledim ve Yıl ve Değer'leri nümerik hale getirdim.
data <- data[-1, ] 

data_cleaned <- data %>%
  drop_na()


data_cleaned <- data_cleaned %>%
  mutate(
    Value = as.numeric(Value),
    Year = as.numeric(Year)
  )

#Bu grafikte yalnızca güncel verileri kullanacağım bu verideki son yıl olan 2022'yi filtreledim.
data_2022 <- data_cleaned %>%
  filter(Year == 2022)

#Kullanacağım indicator kodları seçtim, bu kodlara göre mutate fonksiyonu kullanarak cinsiyet ve istihdam türünü belirten yeni iki sütun ekledim. 

filtered_data_2022 <- data_2022 %>%
  filter(`Indicator Code` %in% c("SL.EMP.VULN.MA.ZS", "SL.EMP.VULN.FE.ZS", "SL.TLF.PART.MA.ZS", "SL.TLF.PART.FE.ZS", "SL.FAM.WORK.MA.ZS", "SL.FAM.WORK.FE.ZS")) %>%
  mutate(Sex = ifelse(`Indicator Code` %in% c("SL.EMP.VULN.FE.ZS", "SL.TLF.PART.FE.ZS", "SL.FAM.WORK.FE.ZS"), "Kadın", "Erkek"),
         Employment_Type = case_when(
           `Indicator Code` %in% c("SL.EMP.VULN.MA.ZS", "SL.EMP.VULN.FE.ZS") ~ "Kırılgan İstihdam",
           `Indicator Code` %in% c("SL.TLF.PART.MA.ZS", "SL.TLF.PART.FE.ZS") ~ "Yarı Zamanlı İstihdam",
           `Indicator Code` %in% c("SL.FAM.WORK.MA.ZS", "SL.FAM.WORK.FE.ZS") ~ "Aile İşçiliği"
         ))


#Barplotla görselleştirdim
ggplot(filtered_data_2022, aes(x = Employment_Type, y = Value, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Kadın ve Erkeklerin İş Türlerine Göre Karşılaştırılması",
       x = "",
       y = "%",
       fill = "Cinsiyet") +
  scale_fill_manual(values = c("Erkek" = "darkgoldenrod1", "Kadın" = "#9999FF")) +
  theme_minimal()

```
![4-Kirilgan_emek](https://github.com/MaykothePsycho/istihdamda_kadin_erkek_esitsizligi/assets/121159197/e8824045-7bf8-499c-98f0-2705e06d54bc)

# Dikey Ayrışma ve Cam Tavan

Kadınların üst düzey işlere erişmesi ise erkeklere göre çok daha zor. Türkiye’de 100 kadından yalnızca 13’ü üst düzey pozisyonlarda görev yapıyor. Birçok işyerinde, kadınların ağırlıklı olarak çalıştıkları işlerde dahi kadınların yönetici pozisyonlara gelemedikleri görülüyor. Aynı şekilde bakanlık veya milletvekili gibi kamu idaresi konusundaki yüksek statülü işlerde de kadınların oranları vahim derecede az.

```
#YÖNETİCİ ROLLERDE KADINLAR VE ERKEKLER

# Yönetici rolleri için kullanılan göstergeleri filtreledim.
filtered_mng <- data_cleaned %>%
  filter(`Indicator Code` %in% c("SG.GEN.MNST.ZS", "SG.GEN.PARL.ZS", "IC.FRM.FEMM.ZS", "IC.FRM.FEMO.ZS"))

# #Bu grafikte yalnızca güncel verileri kullanacağım bu verilerdeki son yıl olan 2019'yi filtreledim.
filtered_by_mng <- filtered_mng %>%
  filter(Year == 2019)

# Değerler sadece kadınları gösterdiği için, erkeklerin oranını hesapladım ve daha kolay çalışabilmek için kadın ve erkek diye iki sütun ekledim.
filtered_by_mng <- filtered_by_mng %>%
  mutate(Kadın = as.numeric(Value),
         Erkek = 100 - Kadın)


# Görselleştirmeye uygun formata getirmek için yüzdelik olarak birleştirdim.
filtered_by_mng_long <- tidyr::pivot_longer(filtered_by_mng, cols = c(Kadın, Erkek), names_to = "Cinsiyet", values_to = "Percentage")

# Cinsiyet sütununu factor hale getirdim.
filtered_by_mng_long$Cinsiyet <- factor(filtered_by_mng_long$Cinsiyet, levels = c("Kadın", "Erkek"))

# Indicator code sütununu factor hale getirerek, Türkçe isimlerini yerleştirdin.
filtered_by_mng_long$`Indicator Code` <- factor(filtered_by_mng_long$`Indicator Code`,
                                                labels = c("Bakanlık Seviyesi Pozisyonlar",
                                                           "Milletvekilleri",
                                                           "Şirket Yöneticileri",
                                                           "Şirket Sahipleri"))

# Bar plot ile görselleştirdim.
ggplot(filtered_by_mng_long, aes(x = `Indicator Code`, y = Percentage, fill = Cinsiyet)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("Kadın" = "#9999FF", "Erkek" = "darkgoldenrod1")) +
  labs(title = "Yönetici Pozisyonu İşlerde Kadınlar ve Erkeklerin Karşılaştırılması",
       x = "",
       y = "%") +
  theme_minimal()

```

![5-Yonetici](https://github.com/MaykothePsycho/istihdamda_kadin_erkek_esitsizligi/assets/121159197/27551a7e-b73a-4f15-ba7f-855bb9530a52)

# İstihdamda kadın erkek eşitliği için yasal düzenlemeler

Toplumsal cinsiyet eşitliğini izlemede önemli bir dönüm noktası, 1990’ların başından itibaren uluslararası kuruluşlarca bu konudaki göstergelerin ve ensekslerin oluşturulmasıdır. Bu endekslerden biri de üreme sağlığı, güçlenme ve işgücü piyasasına dair göstergelerin kullanıldığı Toplumsal Cinsiyet Eşitsizlik Endeksi.
İstihdam alanında izleme ve değerlendirme yapabilmek için bu göstergelerden faydalanılır. Bu göstergelere baktığımızda Türkiye’de 2000'lı yılların başında kadın istihdamı alanında olumlu yöndeki değişiklikleri görmek mümkün. Aşağıdaki grafikte bazı önemli göstergelerin zaman çizelgesi yer almaktadır.


```
#Kadın istihdamına yönelik için bazı düzenlemeler


# İlgili göstergeleri filtreledim.
filtered_timeline <- data_cleaned %>%
  filter(`Indicator Code` %in% c("SG.LAW.NODC.HR", "SG.LAW.EQRM.WK", "SG.GET.JOBS.EQ", "SG.LAW.EQRM.WK", "SG.DNG.WORK.DN.EQ", "SG.DML.PRGW"))

# Türkçe açıklamalar için yeni bir sütun ekledim.
filtered_timeline <- filtered_timeline %>%
  mutate(Explanation = case_when(
    `Indicator Code` == "SG.LAW.NODC.HR" ~ "İş yerinde toplumsal cinsiyete yönelik ayrımcılığı engelleyen yasa",
    `Indicator Code` == "SG.LAW.EQRM.WK" ~ "Kadınlar ve erkekler için eşit işe eşit ücreti zorunlu tutan yasa",
    `Indicator Code` == "SG.GET.JOBS.EQ" ~ "Kadınların erkeklerle aynı şekilde iş bulabilmesi",
    `Indicator Code` == "SG.DNG.WORK.DN.EQ" ~ "Kadınların tehlikeli görülen bir işte erkeklerle aynı şekilde çalışabilmesi",
    `Indicator Code` == "SG.DML.PRGW" ~ "Hamile kadınların işten çıkarılmasının yasaklanması"
  ))

# Görselleştirdim
ggplot(filtered_timeline, aes(x = Year, y = Explanation, color = factor(Value))) +
  geom_point() +
  scale_color_manual(values = c("0" = "grey", "1" = "#9999FF"), guide = FALSE) +  # Customize colors for 0s and 1s
  labs(title = "Kadın istihdamı konusunda yapılan bazı yasal düzenlemeler",
       x = "",
       y = "") +
  theme_minimal()

```

![6-Sozlesme](https://github.com/MaykothePsycho/istihdamda_kadin_erkek_esitsizligi/assets/121159197/545ff5b7-d2fa-4c0f-9242-d3f8b6af9dba)
