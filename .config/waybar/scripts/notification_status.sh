#!/bin/bash

# Obtém a saída do swaync-client
output=$(swaync-client -c 2>/dev/null)

# Verifica se a saída é um número diretamente ou um JSON com "count"
if [[ "$output" =~ ^[0-9]+$ ]]; then
    # Se for um número, usa diretamente
    unread="$output"
elif [[ "$output" =~ ^\{.*\}$ ]]; then
    # Se for JSON, extrai o campo "count"
    unread=$(echo "$output" | jq '.count' 2>/dev/null)
else
    # Caso contrário, assume 0 (sem notificações ou erro)
    unread=0
fi

# Garante que unread é um número válido, senão usa 0
if ! [[ "$unread" =~ ^[0-9]+$ ]]; then
    unread=0
fi

# Exibe o ícone com base no número de notificações
if [ "$unread" -gt 0 ]; then
    echo "✅ $unread"
else
    echo "🔔"
fi
