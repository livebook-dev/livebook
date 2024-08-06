# How to use Nginx to serve Livebook over HTTPS

This guide shows you how to use Nginx to serve Livebook over HTTPS.

## Prerequisites

- Nginx installed
- Livebook installed
- Both Livebook and Nginx running on the same machine or within the same network
- SSL certificate and key files

## Nginx configuration

Use the following Nginx config file as a starting point:

```nginx
http {
    server {
        listen 443 ssl;
        server_name your_domain_or_ip;  # e.g., livebook.example.com

        ssl_certificate /path/to/your/ssl_certificate.crt;  # e.g., /etc/nginx/ssl/livebook.crt
        ssl_certificate_key /path/to/your/ssl_certificate.key;  # e.g., /etc/nginx/ssl/livebook.key

        location / {
            proxy_pass http://livebook_ip:livebook_port;  # e.g., http://172.20.0.3:8080 (Livebook's default port is 8080)
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
        }
    }

    server {
        listen 80;
        server_name your_domain_or_ip;  # e.g., livebook.example.com
        return 301 https://$host$request_uri;
    }
}

events {}
```

Note that there's no need to configure a proxy for Livebook's iframe port (the default is 8081)
because when accessing Livebook over HTTPS, it loads its iframe from
https://livebookusercontent.com, not from your installed Livebook instance.