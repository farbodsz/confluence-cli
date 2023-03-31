FROM haskell:9.2.4

WORKDIR /opt/confluence-cli
RUN stack update

# Install dependencies
COPY ./package.yaml ./stack.yaml ./stack.yaml.lock /opt/confluence-cli/
RUN stack build --only-dependencies -j4

# Install application
COPY . /opt/confluence-cli/
RUN stack install

CMD ["confluence"]
