                    
                            ### ---------- Bioconductor QUIZZES WEEK 1  ---------- ###


# 1 -----------------------------------------------------------------------

#Use the AnnotationHub package to obtain data on "CpG Islands" in the human genome.
#Question: How many islands exists on the autosomes?

ah <- AnnotationHub()
ah <- subset(ah, species == "Homo sapiens")

cpg <- query(ah, "CpG Islands")[[1]]

cpg_autosomes <- subset(cpg, nchar(as.character(seqnames(cpg))) <= 5)))
cpg_autosomes <- subset(cpg_autosomes, seqnames != "chrY" & seqnames != "chrX"))

length(cpg_autosomes)
[1] 26641



# 2 -----------------------------------------------------------------------

# Question: How many CpG Islands exists on chromosome 4

ah <- AnnotationHub()
qah <- ah[ah$genome == "hg19" & ah$title == "CpG Islands"]
cpg <- qah[[1]]

subset(cpg, as.character(seqnames) == "chr4")

GRanges object with 1031 ranges and 1 metadata column



# 3 -----------------------------------------------------------------------

#Obtain the data for the H3K4me3 histone modification for the H1 cell line from Epigenomics Roadmap, using AnnotationHub.
#Subset these regions to only keep regions mapped to the autosomes (chromosomes 1 to 22).
#Question: How many bases does these regions cover?

ah <- AnnotationHub()
ah <- subset(ah, species == "Homo sapiens")

qah <- query(ah, c("H3K4me3", "E003")) # E003 is a Roadmap Epigenetics code for the H1 cell line!!! 
ex3 <- qha[["AH29884"]] # we use NarrowPeak dataset

ex3_autosomes <- subset(ex3, seqnames != "chrY" & seqnames != "chrX")

sum(width(ex3_autosomes))
[1] 41135164


# 4 -----------------------------------------------------------------------

# Obtain the data for the H3K27me3 histone modification for the H1 cell line from Epigenomics Roadmap, using the AnnotationHub package.
# Subset these regions to only keep regions mapped to the autosomes. In the return data, each region has an associated "signalValue".
# Question: What is the mean signalValue across all regions on the standard chromosomes?

ah <- AnnotationHub()
ah <- subset(ah, species == "Homo sapiens")

qah2 <- query(ah, c("H3K27me3", "E003"))
ex4 <- qah2[["AH29892"]]

ex4_autosomes <- subset(ex4, seqnames != "chrY" & seqnames != "chrX")
mean(ex4_autosomes$signalValue)
[1] 4.770728



# 5 -----------------------------------------------------------------------

#Bivalent regions are bound by both H3K4me3 and H3K27me3.
#Question: Using the regions we have obtained above, how many bases on the standard chromosomes are bivalently marked?

bivalent <- intersect(ex3_autosomes, ex4_autosomes, ignore.strand = T)
sum(width(bivalent))
[1] 10289096


# 6 -----------------------------------------------------------------------

#We will examine the extent to which bivalent regions overlap CpG Islands.
#Question: how big a fraction (expressed as a number between 0 and 1) of the bivalent regions, overlap one or more CpG Islands?

cpg_overlap <- findOverlaps(bivalent, cpg_autosomes)
length(unique(queryHits(cpg_overlap))) / length(bivalent)
[1] 0.5383644


# 7 -----------------------------------------------------------------------
# Question:How big a fraction (expressed as a number between 0 and 1) of the bases which are part of CpG Islands,are also bivalent marked?

sum(width(intersect(cpg_autosomes, bivalent, ignore.strand = T))) / sum(width(cpg_autosomes))
[1] 0.2337406    # wrong !!!!!!!


# 8 -----------------------------------------------------------------------

# How many bases are bivalently marked within 10kb of CpG Islands?
# Tip: consider using the "resize()"" function.

cpg_resized <- resize(cpg_autosomes, width = 20000 + width(cpg_autosomes), fix = "center")
intersect(bivalent, cpg_resized)

sum(width(subsetByOverlaps(bivalent, cpg_resized)))
[1] 9785641 ## (coursera 9782086)



# 9 -----------------------------------------------------------------------
#Question: How big a fraction (expressed as a number between 0 and 1) of the human genome is contained in a CpG Island?

sum(width(cpg))/(3000*10^6)
[1] 0.007280914 ## (coursera 0.007047481)

