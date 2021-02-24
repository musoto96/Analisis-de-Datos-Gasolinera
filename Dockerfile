FROM r-base
WORKDIR /shell
COPY . ./
RUN chmod +x bootstrap.sh
RUN ./bootstrap.sh
ENV PORT 8080
CMD ["Rscript", "gasolina.R"]