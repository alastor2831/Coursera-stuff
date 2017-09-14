
### ---------- Bioconductor QUIZZES WEEK 2  ---------- ###


# 1 -----------------------------------------------------------------------

library(BSgenome)
biocLite("BSgenome.Hsapiens.UCSC.hg19")
library("BSgenome.Hsapiens.UCSC.hg19")
seqnames(Hsapiens)c
chr22 <- Hsapiens$chr22
total <- length(chr22) - as.numeric(letterFrequency(chr22, "N"))

letterFrequency(chr22, "GC") / total
      G|C 
0.4798807



# 2 -----------------------------------------------------------------------
#Question: What is mean GC content of H3K27me3 "narrowPeak" regions from Epigenomics Roadmap from the H1 stem cell line on chr 22.

H1.chr22 <- subset(ex4, seqnames == "chr22") # from quiz 1
H1.chr22 <- sort(H1.chr22)

vi <- Views(Hsapiens, H1.chr22) # map the GRanges of H3K27 chr 22 dataset on the DNAString of the Human Genome

mean(letterFrequency(vi, "GC", as.prob = T))
[1] 0.528866


# 3 -----------------------------------------------------------------------
# The "narrowPeak" regions includes information on a value they call "signalValue".
# Question: What is the correlation between GC content and "signalValue" of these regions (on chr22)?

signalValue <- mcols(vi)$signalValue
GC <- as.numeric(letterFrequency(vi, "GC", as.prob = T))
cor(signalValue, GC)
[1] 0.004467924



# 4 -----------------------------------------------------------------------
# The "narrowPeak" regions are presumably reflective of a ChIP signal in these regions. To confirm this, we want to obtain the "fc.signal"
# data from AnnotationHub package on the same cell line and histone modification. This data represents a vector of fold-change enrichment 
# of ChIP signal over input.
# Question: what is the correlation between the "signalValue" of the "narrowPeak" regions and the average "fc.signal" across the same
# regions?
# Clarification: First compute the average "fc.signal" for across each region, for example using "Views"; this yields a single number of
# each region. Next correlate these numbers with the "signalValue" of the "narrowPeaks".


query(ah, c("H3K27me3", "E003", "fc.signal"))
chip_fc <- query(ah, c("H3K27me3", "E003", "fc.signal"))[[1]] # return a BigWig file
rle_fc_signal <- import(chip_fc, which = H1.chr22, as = "Rle") # read only the chr22 as a GRange object

fc_score_chr22 <- rle_fc_signal[H1.chr22] # return an Rle list of 4068 elements
fc_mean <- mean(fc_score_chr22) # vector of length 4068 with the score avarege for each ranges in H1.chr22
cor(fc_mean, signalValue) # calculate the corr with SignalValue
[1] 0.9149614

### alternative method using Views

vi2 <- Views(rle_fc_signal$chr22, as(H1.chr22, "RangesList")$chr22)
fc_mean2 <- viewMeans(vi2) # equivalent to use mean() from base R
cor(fc_mean2, signalValue)
[1] 0.9149614

mean(as.numeric(rle_fc_signal$chr22[start(H1.chr22[1]):end(H1.chr22[1])]))

# 5 -----------------------------------------------------------------------
# Question: How many bases on chr22 have an fc.signal greater than or equal to 1?

fc_1 <- fc_signal


    # 10907433 # coursera correct answer


# 6 -----------------------------------------------------------------------

chip_fc2 <- query(ah, c("H3K27me3", "E055", "fc.signal"))[[1]]

E055_narrowPeak <- query(ah, c("H3K27me3", "E055", "narrow"))[[1]]
E055.chr22 <- subset(E055_narrowPeak, seqnames == "chr22")
E055.chr22 <- sort(E055.chr22)

rle_fc_signal2 <- import(chip_fc2, which = H1.chr22, as = "Rle")

vi3 <- Views(rle_fc_signal2$chr22, as(H1.chr22, "RangesList")$chr22)


logical_vi2 <- runValue(vi2) > 0.5
