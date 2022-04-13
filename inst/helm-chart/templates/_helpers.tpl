{{/*
Ensure at least one disk is given
*/}}
{{- define "biockubeinstall.manualDiskBlock" -}}
{{- if .Values.persistence.gcpPdName -}}
gcePersistentDisk:
  pdName: {{ .Values.persistence.gcpPdName }}
  fsType: ext4
{{- else -}}
{{- if .Values.persistence.azurePdHandle -}}
csi:
  driver: disk.csi.azure.com
  readOnly: false
  volumeHandle: {{ .Values.persistence.azurePdHandle }}
  volumeAttributes:
    fsType: ext4
{{- end -}}
{{- end -}}
{{- end -}}
