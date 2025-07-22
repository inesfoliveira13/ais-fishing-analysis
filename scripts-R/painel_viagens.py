import streamlit as st
import pandas as pd
import plotly.express as px

# Carregar os dados
df = pd.read_csv("viagens_validas.csv")

# Converter colunas de data/hora
df['Data'] = pd.to_datetime(df['Data'])

# Filtros na barra lateral
st.sidebar.title("Filtros")

# Filtrar embarcação
embarcacao = st.sidebar.selectbox("Selecione uma embarcação", sorted(df['Code'].unique()))

# Filtrar intervalo de datas
min_data = df['Data'].min().date()
max_data = df['Data'].max().date()
data_inicio, data_fim = st.sidebar.date_input("Intervalo de datas", [min_data, max_data])

# Filtrar dados pela embarcação e intervalo de datas
df_filt = df[
    (df['Code'] == embarcacao) &
    (df['Data'] >= pd.to_datetime(data_inicio)) &
    (df['Data'] <= pd.to_datetime(data_fim))
]

# Eliminar viagens com apenas um registo
viagens_validas = df_filt['TRIP_ID'].value_counts()
viagens_validas = viagens_validas[viagens_validas > 1].index
df_filt = df_filt[df_filt['TRIP_ID'].isin(viagens_validas)]

# Mostrar todas as viagens encontradas
viagens = sorted(df_filt['TRIP_ID'].unique())
st.sidebar.write(f"Viagens com >1 registo encontradas: {len(viagens)}")

# Mostrar dados por viagem
for viagem in viagens:
    df_viagem = df_filt[df_filt['TRIP_ID'] == viagem] 

    # Capturar informações da viagem
    data_viagem = df_viagem.iloc[0]['Data'].date()
    porto_partida = df_viagem.iloc[0]['which.PORT'] if pd.notna(df_viagem.iloc[0]['which.PORT']) else "Desconhecido"
    porto_chegada = df_viagem.iloc[-1]['which.PORT'] if pd.notna(df_viagem.iloc[-1]['which.PORT']) else "Desconhecido"

    with st.expander(f" {viagem}"):
        st.write(f"**Data da Viagem:** {data_viagem}")
        st.write(f"**Porto de Partida:** {porto_partida}")
        st.write(f"**Porto de Chegada:** {porto_chegada}")

        st.write("#### Velocidade vs Tempo Relativo")
        fig = px.line(df_viagem, x='Tempo_Relativo_Horas', y='Velocidade',
                      markers=True, title=None,
                      labels={'Tempo_Relativo_Horas': 'Duração da Viagem (h)', 'Velocidade': 'Velocidade (nós)'})
        st.plotly_chart(fig, use_container_width=True, key=f"plot_{viagem}")

        st.write("#### Resumo da Viagem")
        st.metric("Velocidade Média (nós)", round(df_viagem['Velocidade'].mean(), 2))
        st.metric("Duração da Viagem (h)", round(df_viagem['Tempo_Relativo_Horas'].max(), 2))


# python -m streamlit run painel_viagens.py