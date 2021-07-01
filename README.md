# OCC (One-Class Classification)

One-class classification is an active area within machine learning that can address the situation of imbalanced data with unequally weighted class distributions. In contrast to standard supervised classification methods which aim to differentiate between two or more well-sampled classes, one-class classification attempts to describe a single class of objects called the target class. In such problems, examples of non-target (outlier) objects are often severely undersampled or even completely absent. Usual examples are found in the biomedical field when learning on imbalanced data representing healthy versus ill cases. Heavily skewed class priors and misclassification costs negatively impact standard multi-class classification, and usual evaluation criteria such as crossvalidation accuracy become problematic.

One-class classification has a wide range of applications from monitoring to financial credit scoring, fraud detection, and cybersecurity. In network systems, one-class classifiers can be trained to detect suspicious patterns, activity, or intrusions. In the biomedical field, one-class classifiers become essential when predicting the diagnosis or prognostic outcome of patients in situations where the number of ill cases compared to healthy cases is scant. In the field of finance and banking, common applications are in detecting (and blocking) fraudulent transactions in real time, as well as in automating the process of credit scoring for deciding on how likely a customer will be able to pay a future loan, especially that often relatively few examples exist of cases that have defaulted in the past.

Below are HTML demo files of some classifiers used for OCC; recent work is indicated by (*)<br>

<a class="external reference" href="https://wz7kfejx.github.io/OCC/demo_gaussocc.html">Gaussian one-class classifier</a> <br>
<a class="external reference" href="https://wz7kfejx.github.io/OCC/demo_parzenocc.html">Parzen one-class classifier</a> <br>
<a class="external reference" href="https://wz7kfejx.github.io/OCC/demo_knnocc.html">K-nn one-class classifier</a> <br>
<a class="external reference" href="https://wz7kfejx.github.io/OCC/demo_svmocc.html">SVM one-class classifier</a> <br>
<a class="external reference" href="https://wz7kfejx.github.io/OCC/demo_kmeansocc.html">K-means one-class classifier</a> <br> 
<a class="external reference" href="https://wz7kfejx.github.io/OCC/demo_somocc.html">SOM one-class classifier</a> <br>
<a class="external reference" href="https://wz7kfejx.github.io/OCC/demo_gmmocc.html">Gaussian mixture model one-class classifier</a> <br>
<a class="external reference" href="https://wz7kfejx.github.io/OCC/demo_rdocc_mocc.html">RDOCC & MOCC*</a><br>
<a class="external reference" href="https://wz7kfejx.github.io/OCC/demo_median_transform.html">Median transform (preprocessing)*</a> <br>

A paper on RDOCC & MOCC (in addition to experiments with isolation forest, SVM, and local outlier factor): 
<a class="external reference" href="https://wz7kfejx.github.io/OCC/rdocc_mocc_2.pdf">RDOCC/MOCC*</a><br>

A case study for building a network intrusion detection system for the KDD'99 (DARPA-based) dataset which is one of the few publicly available data sets for network-based anomaly detection systems: <br>

Description: <a class="external reference" href="https://wz7kfejx.github.io/OCC/Network_IDS.html">Network Intrusion Detection Problem</a> <br>
Demo file: <a class="external reference" href="https://wz7kfejx.github.io/OCC/demo_network_IDS.html">Network Intrusion Detection System (KDD)</a> <br>
Demo file: <a class="external reference" href="https://wz7kfejx.github.io/OCC/demo_NSL_KDD.html">Network Intrusion Detection System using OCC (NSL-KDD)*</a> <br>
