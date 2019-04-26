FROM mariadb:10

# See https://serverfault.com/q/683605/54523
ARG TZ
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

EXPOSE 3306