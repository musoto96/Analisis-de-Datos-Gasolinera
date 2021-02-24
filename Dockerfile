FROM r-base
WORKDIR /shell
COPY . ./
RUN chmod +x bootstrap.sh
RUN ./bootstrap.sh
CMD ["Rscript", "gasolina.R"]