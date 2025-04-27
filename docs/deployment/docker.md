# Docker

There are two main use cases to deploy Livebook in the cloud. The first is to read and write notebooks in the cloud, instead of your machine. The second is to deploy notebooks as applications.

## Livebook in the cloud

You can deploy Livebook inside your infrastructure using Docker. The Dockerfile below provides a great starting point:

```dockerfile
FROM ghcr.io/livebook-dev/livebook

# Configure your port accordingly
ENV LIVEBOOK_PORT=7860
EXPOSE 7860

# If you have a persistent volume, configure it here
ENV LIVEBOOK_DATA_PATH="/data"
USER root
RUN mkdir -p /data
RUN chmod 777 /data
```

We also recommend setting the `LIVEBOOK_PASSWORD` environment variable to a secret value. If it is not set, you will find the token to access Livebook in the logs. See all other supported [environment variables](../../README.md#environment-variables) to learn more.

If you want to run several Livebook instances behind a load balancer, you need to enable clustering. See the [Clustering](clustering.md) section.

If you plan to limit access to your Livebook via a proxy, we recommend leaving the "/public" route of your instances still public. This route is used for integration with the [Livebook Badge](https://livebook.dev/badge/) and other conveniences.

### Docker compose

If using Docker Compose the following template is a good starting point:

```yml
services:
  livebook:
    image: ghcr.io/livebook-dev/livebook
    ports:
      - 8090:8090
      - 8091:8091
    environment:
      - LIVEBOOK_PORT=8090
      - LIVEBOOK_IFRAME_PORT=8091
```

### Kubernetes

If using k8s the following template is a good starting point. It includes a load balancer and preset clustering:

```yml
apiVersion: v1
kind: Service
metadata:
  name: livebook-headless
spec:
  clusterIP: None
  selector:
    app: livebook

---

apiVersion: v1
kind: Service
metadata:
  name: livebook-loadbalancer
spec:
  type: ClusterIP
  ports:
    - port: 8080
      targetPort: 8080
  selector:
    app: livebook

---

apiVersion: apps/v1
kind: Deployment
metadata:
  name: livebook
spec:
  # When deploying Livebook for authoring notebooks to Kubernetes,
  # the number of replicas must be 1, since Livebook considers you
  # will assign one instance per user.
  replicas: 1
  selector:
    matchLabels:
      app: livebook
  template:
    metadata:
      labels:
        app: livebook
    spec:
      containers:
        - name: livebook
          image: ghcr.io/livebook-dev/livebook:latest
          ports:
            - containerPort: 8080
          env:
            - name: POD_IP
              valueFrom:
                fieldRef:
                  fieldPath: status.podIP
            - name: POD_NAMESPACE
              valueFrom:
                fieldRef:
                  fieldPath: metadata.namespace
            - name: LIVEBOOK_NODE
              value: "livebook@$(POD_IP)"
            - name: LIVEBOOK_CLUSTER
              value: "dns:livebook-headless.$(POD_NAMESPACE).svc.cluster.local"
            - name: LIVEBOOK_PASSWORD
              valueFrom:
                secretKeyRef:
                  name: livebook-secret
                  key: LIVEBOOK_PASSWORD
            - name: LIVEBOOK_SECRET_KEY_BASE
              valueFrom:
                secretKeyRef:
                  name: livebook-secret
                  key: LIVEBOOK_SECRET_KEY_BASE
            - name: LIVEBOOK_COOKIE
              valueFrom:
                secretKeyRef:
                  name: livebook-secret
                  key: LIVEBOOK_COOKIE

---

apiVersion: v1
kind: Secret
metadata:
  name: livebook-secret
  namespace: livebook-namespace
type: Opaque
data:
  LIVEBOOK_PASSWORD: <base64_encoded_password>
  LIVEBOOK_SECRET_KEY_BASE: <base64_encoded_password>
  LIVEBOOK_COOKIE: <base64_encoded_password>
```

The setup above does not set up a data directory, which means once you restart the instance, any configuration will be lost. If you have a persistent volume, you can point the `LIVEBOOK_DATA_PATH` environment variable to it.

## Deploy notebooks as applications

It is possible to deploy any notebook as an application in Livebook. Inside the notebook, open up the Application pane on the sidebar (with a rocket icon), click "Manual Docker deployment", and follow the required steps.

If you are using [Livebook Teams](https://livebook.dev/teams/), you can also deploy with the click of a button by running Livebook servers inside your infrastructure. To get started, open up Livebook and click "Add Organization" on the sidebar. Once completed, open up the Application pane on the sidebar (with a rocket icon), click "Deploy with Livebook Teams", and follow the deployment steps.

The deployment steps will show you to deploy your notebooks within Docker, Fly.io, and Kubernetes. This is effectively done by setting the `LIVEBOOK_TEAMS_AUTH`, which configures Livebook to run as a read-only instance connected to Livebook Teams.

Livebook Teams also support airgapped deployments, pre-configured environment variables, shared team secrets, file storages, and more.
