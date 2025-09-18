#!/bin/bash

# Obtém notificações em espera (waiting)
unread=$(dunstctl count waiting)

# Garante que unread é um número válido, senão usa 0
if ! [[ "$unread" =~ ^[0-9]+$ ]]; then
    unread=0
fi

# Se não há notificações em espera, verifica o histórico
if [ "$unread" -eq 0 ]; then
    unread=$(dunstctl count history)
    if ! [[ "$unread" =~ ^[0-9]+$ ]]; then
        unread=0
    fi
fi

# Exibe o ícone com base no número de notificações
if [ "$unread" -gt 0 ]; then
    echo " $unread"
else
    echo ""
fi
